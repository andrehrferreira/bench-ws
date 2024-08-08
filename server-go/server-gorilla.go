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
	clientsToWaitFor    = 300
	waitTimeBetweenTests = 20 * time.Second
)

var (
	clients    sync.Map // sync.Map para melhor performance concorrente
	upgrader   = websocket.Upgrader{
		CheckOrigin: func(r *http.Request) bool {
			return true
		},
	}
	register   = make(chan *websocket.Conn)
	unregister = make(chan *websocket.Conn)
	broadcast  = make(chan Message)
)

type Message struct {
	name    string
	message []byte
	msgType int
}

func main() {
	http.HandleFunc("/", handleConnections)

	go handleMessages()

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
		return
	}
	defer ws.Close()

	register <- ws

	name := fmt.Sprintf("Client%d", lenClients()+1)
	clients.Store(ws, name)
	fmt.Printf("%s connected (%d remain)\n", name, clientsToWaitFor-lenClients())

	if lenClients() == clientsToWaitFor {
		sendReadyMessage()
	}

	for {
		msgType, message, err := ws.ReadMessage()
		if err != nil {
			unregister <- ws
			break
		}
		broadcast <- Message{name, message, msgType}
	}
}

func handleMessages() {
	for {
		select {
		case ws := <-register:
			clients.Store(ws, fmt.Sprintf("Client%d", lenClients()+1))

		case ws := <-unregister:
			clients.Delete(ws)
			fmt.Printf("Client disconnected, %d clients remaining\n", lenClients())

		case msg := <-broadcast:
			broadcastMessage(msg.name, msg.message, msg.msgType)
		}
	}
}

func broadcastMessage(name string, message []byte, messageType int) {
	msg := fmt.Sprintf("Message from %s: %s", name, string(message))
	clients.Range(func(client, _ interface{}) bool {
		c := client.(*websocket.Conn)
		if err := c.WriteMessage(messageType, []byte(msg)); err != nil {
			log.Printf("error: %v", err)
			c.Close()
			clients.Delete(c)
		}
		return true
	})
}

func sendReadyMessage() {
	fmt.Println("All clients connected")
	time.AfterFunc(100*time.Millisecond, func() {
		fmt.Println("Starting benchmark")
		clients.Range(func(client, _ interface{}) bool {
			c := client.(*websocket.Conn)
			err := c.WriteMessage(websocket.TextMessage, []byte("ready"))
			if err != nil {
				log.Printf("error: %v", err)
				c.Close()
				clients.Delete(c)
			}
			return true
		})
	})
}

func lenClients() int {
	length := 0
	clients.Range(func(_, _ interface{}) bool {
		length++
		return true
	})
	return length
}
