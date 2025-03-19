//go:build !solution

package main

import (
	"bufio"
	"context"
	"flag"
	"fmt"
	"log"

	//"log"
	"os"
	"os/signal"
	"sync"
	"syscall"

	"github.com/gorilla/websocket"
)

func main() {
	addr := flag.String("addr", "ws://ws.ifelse.io", "адрес сервера")
	flag.Parse()

	con, _, _ := websocket.DefaultDialer.Dial(*addr, nil)
	defer con.Close()

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	var wg sync.WaitGroup
	go handle(&wg, cancel)
	wg.Add(1)
	go read(&wg, ctx, con)

	mes := make(chan string)
	sc := bufio.NewScanner(os.Stdin)
	go scan(sc, mes)

	for {
		select {
		case <-ctx.Done():
			return
		case message := <-mes:
			err := con.WriteMessage(websocket.TextMessage, []byte(message))
			if err != nil {
				log.Fatal("error")
			}
		}
	}
}

func scan(scanner *bufio.Scanner, messages chan string) {
	for scanner.Scan() {
		messages <- scanner.Text()
	}
}

func read(wg *sync.WaitGroup, ctx context.Context, conn *websocket.Conn) {
	defer wg.Done()
	for {
		select {
		case <-ctx.Done():
			return
		default:
			_, message, _ := conn.ReadMessage()
			fmt.Printf("%s", message)
		}
	}
}

func handle(wg *sync.WaitGroup, cancel context.CancelFunc) {
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)
	<-sigChan
	cancel()
	wg.Wait()
}
