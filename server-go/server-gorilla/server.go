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
	port                 = 3005
	clientsToWaitFor     = 32 // Ajustado para 32 clientes
	waitTimeBetweenTests = 20 * time.Second
)

var (
	clients  = make(map[*websocket.Conn]*Client)
	mu       sync.Mutex // Mutex para sincronizar o acesso ao mapa clients
	upgrader = websocket.Upgrader{
		CheckOrigin: func(r *http.Request) bool {
			return true
		},
	}
	broadcast = make(chan Message)
)

type Client struct {
	conn *websocket.Conn
	mu   sync.Mutex // Mutex para sincronizar o acesso à conexão WebSocket
	send chan Message
}

type Message struct {
	name        string
	message     []byte
	messageType int
}

func main() {
	http.HandleFunc("/", handleConnections)

	go handleMessages()

	fmt.Printf("WebSocket server is running on ws://0.0.0.0:%d\n", port)
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

	client := &Client{conn: ws, send: make(chan Message)}

	mu.Lock()
	clients[ws] = client
	name := fmt.Sprintf("Client%d", len(clients))
	fmt.Printf("%s connected (%d remain)\n", name, clientsToWaitFor-len(clients))
	mu.Unlock()

	go handleClientMessages(client)

	if len(clients) == clientsToWaitFor {
		go sendReadyMessage()
	}

	for {
		messageType, message, err := ws.ReadMessage()
		if err != nil {
			mu.Lock()
			delete(clients, ws)
			mu.Unlock()
			break
		}
		broadcast <- Message{name: name, message: message, messageType: messageType}
	}
}

func handleClientMessages(client *Client) {
	for msg := range client.send {
		client.mu.Lock()
		if err := client.conn.WriteMessage(msg.messageType, msg.message); err != nil {
			log.Printf("error: %v", err)
			client.conn.Close()
			mu.Lock()
			delete(clients, client.conn)
			mu.Unlock()
		}
		client.mu.Unlock()
	}
}

func handleMessages() {
	for {
		msg := <-broadcast
		broadcastMessage(msg)
	}
}

func broadcastMessage(msg Message) {
	mu.Lock()
	defer mu.Unlock()
	for _, client := range clients {
		client.send <- msg
	}
}

func sendReadyMessage() {
	fmt.Println("All clients connected")
	time.AfterFunc(100*time.Millisecond, func() {
		fmt.Println("Starting benchmark")
		mu.Lock()
		defer mu.Unlock()
		for _, client := range clients {
			client.send <- Message{messageType: websocket.TextMessage, message: []byte("ready")}
		}
	})
}