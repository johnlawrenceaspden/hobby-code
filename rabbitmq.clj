;; Here we're going to start two separate processes and use them to send
;; messages to one another via rabbitMQ.

;; First we need to install rabbitmq

;; Unfortunately Ubuntu has an old version, which our client library is unable to talk to.
;; Since the protocol has changed, it seems silly to use the old version, especially since rabbit provide a
;; debian package which seems to work fine:

;; Install the latest rabbitmq release (2.3.1 at time of writing)
;; $ wget http://www.rabbitmq.com/releases/rabbitmq-server/v2.3.1/rabbitmq-server_2.3.1-1_all.deb
;; sudo dpkg -i rabbitmq-server_2.3.1-1_all.deb

;; This checks that it's working (woc-desktop is the name of my machine)
;; sudo rabbitmqctl -n rabbit@woc-desktop status
;; Status of node 'rabbit@woc-desktop' ...
;; [{running_applications,[{rabbit,"RabbitMQ","2.3.1"},
;;                         {mnesia,"MNESIA  CXC 138 12","4.4.12"},
;;                         {os_mon,"CPO  CXC 138 46","2.2.4"},
;;                         {sasl,"SASL  CXC 138 11","2.1.8"},
;;                         {stdlib,"ERTS  CXC 138 10","1.16.4"},
;;                         {kernel,"ERTS  CXC 138 10","2.13.4"}]},
;;  {nodes,[{disc,['rabbit@woc-desktop']}]},
;;  {running_nodes,['rabbit@woc-desktop']}]

;; The corresponding java client library is in the central maven repository
;;    <dependency>
;;      <groupId>com.rabbitmq</groupId>
;;      <artifactId>amqp-client</artifactId>
;;      <version>2.3.1</version>
;;    </dependency>

;; And this program can be run with
;; $ mvn -Dclojure.script=rabbitmq.clj clojure:run


(import (com.rabbitmq.client ConnectionFactory Connection Channel QueueingConsumer))

;; We are going into Java-land so there is a certain amount of bollocks to endure:

;; Make a connection factory on the local host
(def connection-factory
     (doto (ConnectionFactory.)
       (.setHost "localhost")))

;; and get it to make you a connection
(def connection (.newConnection connection-factory))

;; get that to make you a channel
(def channel (. connection createChannel))

;; And on that channel, create a queue, named "hello", which is neither durable nor exclusive nor autodelete
(. channel queueDeclare "hello" false false false nil)

;; Now to send a message to that "hello" queue, you need to ask the channel to
;; publish a message to the exchange "", using "hello" as the routing key.
;; Christ knows what the nil is for, and then the message body is the bytes of the legendary string
;; "Hello World!"
(dotimes [ i 10 ]
  (. channel basicPublish "" "hello" nil (. (format "Hello World! (%d)" i) getBytes)))

;; Create a consumer, which will block waiting for messages
(def consumer (new QueueingConsumer channel))

;; Attach it to the channel
(. channel basicConsume "hello" true consumer)

(loop []
  (if-let [delivery (. consumer nextDelivery)]
    (let [str (String. (. delivery getBody))]
      (if (= str "quit")
        (println "exiting...")
        (do 
          (println str)
          (recur))))))


;; this message will kill it
(. channel basicPublish "" "hello" nil (. "quit" getBytes))

