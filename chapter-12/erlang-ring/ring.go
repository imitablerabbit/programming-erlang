package main

import (
	"fmt"
	"time"
	"os"
	"strconv"
)

// Node is a struct which will hold the information that
// is needed in the node function which handles the message
// passing around the ring.
type Node struct {
	In chan string
	// Message will store the message that was passed in via
	// the in channel
	Next *Node // The next node in the ring
}

// Ring is a struct which holds the information on which Node
// is the first
type Ring struct {
	Start *Node
	Message string
	MessageNum int
}

// CreateNodes will initialise the nodes in the ring. It will
// return the starting node in the ring.
func CreateNodes(n int) *Node {
	start := &Node{In: make(chan string)}
	current := start
	for i := 0; i < n-1; i++ {
		n := &Node{In: make(chan string)}
		current.Next = n
		current = n
	}
	current.Next = start
	return start
}

// HandleNode will recieve and send any messages in the ring
// this is how the ring works. When the ring should no longer send
// a message the channel is closed.
func (n *Node) HandleNode() {
	for message := range n.In {
		n.Next.In <- message
	}
}

// HandleStartNode will take care of initialising the ring message
// sending. It will also handle how many times that the message is
// sent. When the message has gone around the ring enough time, this
// node will close all the other nodes.
func (n *Node) HandleStartNode(messageNum int, reply chan bool) {
	for message := range n.In {
		n.Next.In <- message
		messageNum--
		if messageNum <= 0 {
			break
		}
	}
	close(reply)
}

// StartNodes takes a ring of nodes and starts the processes. It also
// creates a channel for the ring to send back information on when it
// is completed. This function will block until the ring has finished
func (r *Ring) StartNodes() {
	reply := make(chan bool)
	go r.Start.HandleStartNode(r.MessageNum, reply)
	for current := r.Start.Next; current != r.Start; current = current.Next {
		go current.HandleNode()
	}
	r.Start.In <- r.Message
	<-reply
}

func main() {
	args := os.Args[1:]
	if len(args) < 3 {
		fmt.Println("Please enter the arguments for the ring:")
		fmt.Println("./ring processNum message messageNum")
		return
	}

	nodes, err := strconv.Atoi(args[0])
	if err != nil {
		fmt.Printf("%v is not a valid processNum, must be a number\n", args[0])
		return
	}
	message := args[1]
	messageNum, err := strconv.Atoi(args[2])
	if err != nil {
		fmt.Printf("%v is not a valid messageNum, must be a number\n", args[2])
		return
	}

	start := CreateNodes(nodes)
	ring := &Ring{start, message, messageNum}
	t0 := time.Now()
	ring.StartNodes()
	t1 := time.Now()
	fmt.Printf("It took %vµs to run\n", t1.Sub(t0).Seconds()*1000000)
}
