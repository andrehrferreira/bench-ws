package main

import (
	"fmt"
	"log"
	"net/http"
	"sync"
	"time"

	"github.com/gorilla/websocket"
)

const (
	port                = 3005
	clientsToWaitFor    = 100
	waitTimeBetweenTests = 20 * time.Second
)

var (
	clients = make(map[*websocket.Conn]bool)
	mu      sync.Mutex // Mutex para sincronizar o acesso ao mapa clients
	upgrader = websocket.Upgrader{
		CheckOrigin: func(r *http.Request) bool {
			return true
		},
	}
)

func main() {
	http.HandleFunc("/", handleConnections)

	fmt.Printf("WebSocket server is running on ws://localhost:%d\n", port)
	err := http.ListenAndServe(fmt.Sprintf(":%d", port), nil)
	if err != nil {
		log.Fatal("ListenAndServe: ", err)
	}
}

func handleConnections(w http.ResponseWriter, r *http.Request) {
	ws, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		log.Fatal(err)
	}
	defer ws.Close()

	mu.Lock()
	clients[ws] = true
	name := fmt.Sprintf("Client%d", len(clients))
	fmt.Printf("%s connected (%d remain)\n", name, clientsToWaitFor-len(clients))
	mu.Unlock()

	if len(clients) == clientsToWaitFor {
		sendReadyMessage()
	}

	for {
		messageType, message, err := ws.ReadMessage()
		if err != nil {
			mu.Lock()
			delete(clients, ws)
			mu.Unlock()
			break
		}
		broadcastMessage(name, message, messageType)
	}
}

func broadcastMessage(name string, message []byte, messageType int) {
	msg := fmt.Sprintf("Message from %s: %s", name, string(message))
	mu.Lock()
	defer mu.Unlock()
	for client := range clients {
		if err := client.WriteMessage(messageType, []byte(msg)); err != nil {
			log.Printf("error: %v", err)
			client.Close()
			delete(clients, client)
		}
	}
}

func sendReadyMessage() {
	fmt.Println("All clients connected")
	time.AfterFunc(100*time.Millisecond, func() {
		fmt.Println("Starting benchmark")
		mu.Lock()
		defer mu.Unlock()
		for client := range clients {
			err := client.WriteMessage(websocket.TextMessage, []byte("ready"))
			if err != nil {
				log.Printf("error: %v", err)
				client.Close()
				delete(clients, client)
			}
		}
	})
}
