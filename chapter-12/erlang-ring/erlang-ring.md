This post is about exercise 3 in chapter 12 of the Programming Erlang - Software for a Concurrent World book. The premise of the exercise is to create a ring of concurrent processes which each send a message around the ring. The message should be passed $M$ times around a ring of $N$ processes. The completion time is then measured for the $M \cdot N$ messages passed and plotted on graphs to compare complexity and efficiency between the languages.

So, for the exercise I decided that Go would be a suitable competitor to Erlang as they can both create processes/goroutines easily and cheaply. Unfortunately, as I am more experienced with Go, the results might be in favour for Go. In order to try and mitigate any discrepancies, I am going to try and create very similar ways of passing the message around the ring.

## Erlang Ring

For the Erlang ring, as I am not sure how to create structures (If you can at all) the best thing I could do is to spawn all the processes and store their Pid's in a list. When first creating the ring, I would access the required ring element by using the lists:nth function along with an index passed along in the message. This was eventually changed to having a spent ring process list and the upcoming process list. The when one process in the ring has been sent a message, it is moved to the spent list. This should increase the performance of the ring operations as it is $O(1)$ rather than $O(n).$ The code for the erlang ring looks somewhat like the following.

```erlang
create_ring(N) -> create_ring(N, []).
create_ring(0, Acc) -> Acc;
create_ring(N, Acc) -> 
    Pid = spawn(ring_new, ring_element, []),
    NewAcc = [Pid|Acc],
    create_ring(N-1, NewAcc).

ring_element() -> 
    receive 
        {From, {[Pid|Ring], Spent, Message, Iter}} ->
            Pid ! {From, {Ring, [Pid|Spent], Message, Iter}},
            ring_element();
        {From, {[], [Pid|Spent], Message, Iter}} ->
            if 
                Iter > 0 ->
                    Pid ! {From, {Spent, [Pid], Message, Iter-1}},
                    ring_element();
                Iter =:= 0 ->
                    From ! {Pid, Message},
                    ring_element()
            end
    end.
```

The erlang ring above produces times like the following:
<html>
    <table>
        <tr>
            <th>Processes</th>
            <th colspan=6>Time taken</th>
        </tr>
        <tr>
            <th></th>
            <th>0</th>
            <th>200</th>
            <th>400</th>
            <th>600</th>
            <th>800</th>
            <th>1000</th>
        </tr>
        <tr>
            <th>10</th>
            <td>0</td>
            <td>0.011404</td>
            <td>0.020294</td>
            <td>0.03002</td>
            <td>0.037855</td>
            <td>0.049618</td>
        </tr>
        <tr>
            <th>100</th>
            <td>0</td>
            <td>0.180322</td>
            <td>0.34868</td>
            <td>0.520021</td>
            <td>0.696981</td>
            <td>0.901608</td>
        </tr>
        <tr>
            <th>1000</th>
            <td>0</td>
            <td>8.550677</td>
            <td>16.838135</td>
            <td>25.270322</td>
            <td>33.418593</td>
            <td>41.724593</td>
        </tr>
    </table>
</html>

The results turned into a graph will look like the following linear graph.

![erlang graph for 1000 processes](/blogs/erlang-ring/images/erlang-1000.png)

## Go Ring

The Go ring is fairly similar to the Erlang ring except that it uses structures to hold the next Node as a pointer. The communication between the nodes is via a channel, which is the closest method to Erlang's send primitive. However, the Go code has been optimised somewhat in that only the start of the ring has to check when the message has gone around the ring, which saves a lot of computation. Thus the Go code will be a fair bit faster. The most important part of this blog is to just show the linear relation between creating more processes and the time taken to complete the message sending. The following is a snippet of the Go code.

```go 
func (r *Ring) StartNodes() {
    reply := make(chan bool)
    go r.Start.HandleStartNode(r.MessageNum, reply)
    for current := r.Start.Next; current != r.Start; current = current.Next {
        go current.HandleNode()
    }
    r.Start.In <- r.Message
    <-reply
}

func (n *Node) HandleNode() {
    for message := range n.In {
        n.Next.In <- message
    }
}
```

The Go ring code above produces times like the following:
<html>
    <table>
        <tr>
            <th>Processes</th>
            <th colspan=6>Time taken</th>
        </tr>
        <tr>
            <th></th>
            <th>0</th>
            <th>200</th>
            <th>400</th>
            <th>600</th>
            <th>800</th>
            <th>1000</th>
        </tr>
        <tr>
            <th>10</th>
            <td>0</td>
            <td>0.003498</td>
            <td>0.007197</td>
            <td>0.009824</td>
            <td>0.012949</td>
            <td>0.01512</td>
        </tr>
        <tr>
            <th>100</th>
            <td>0</td>
            <td>0.032864</td>
            <td>0.065834</td>
            <td>0.092592</td>
            <td>0.123713</td>
            <td>0.14895</td>
        </tr>
        <tr>
            <th>1000</th>
            <td>0</td>
            <td>0.445566</td>
            <td>0.849469</td>
            <td>1.292012</td>
            <td>1.716164</td>
            <td>2.160614</td>
        </tr>
    </table>
</html>

![go graph for 1000 processes](/blogs/erlang-ring/images/go-1000.png)

