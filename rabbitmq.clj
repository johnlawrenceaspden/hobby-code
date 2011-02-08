;; RabbitMQ is a way for different processes to talk to one another.

;; Here we're going to start a process and get it to send
;; messages to itself via rabbitMQ.

;; First we need to install rabbitmq. I'm using Ubuntu 10.10, so that's:

;; $ sudo apt-get install rabbitmq-server

;; Unfortunately Ubuntu has an old version, which the current client library is unable to talk to.

;; Since the protocol has changed, it seems silly to use the old version, especially since rabbit provide a
;; debian package which seems to work fine:

;; To install the latest rabbitmq release (2.3.1 at time of writing)
;; $ wget http://www.rabbitmq.com/releases/rabbitmq-server/v2.3.1/rabbitmq-server_2.3.1-1_all.deb
;; sudo dpkg -i rabbitmq-server_2.3.1-1_all.deb

;; I installed this over the top of the default ubuntu installation and this seems to work.
;; I don't know what happens if you install it out-of-the-blue. Things may not get set up right, although the
;; rabbitmq website seems to indicate that it will be ok.

;; You can check that rabbitMQ is working (woc-desktop is the name of my machine) with

;; $ sudo rabbitmqctl -n rabbit@woc-desktop status

;; Which should respond:

;; Status of node 'rabbit@woc-desktop' ...
;; [{running_applications,[{rabbit,"RabbitMQ","2.3.1"},
;;                         {mnesia,"MNESIA  CXC 138 12","4.4.12"},
;;                         {os_mon,"CPO  CXC 138 46","2.2.4"},
;;                         {sasl,"SASL  CXC 138 11","2.1.8"},
;;                         {stdlib,"ERTS  CXC 138 10","1.16.4"},
;;                         {kernel,"ERTS  CXC 138 10","2.13.4"}]},
;;  {nodes,[{disc,['rabbit@woc-desktop']}]},
;;  {running_nodes,['rabbit@woc-desktop']}]

;; The corresponding java client library is in the central maven repository so
;; you should add this snippet to your pom.xml or the corresponding incantation
;; to your project.clj if you use leiningen:

;;    <dependency>
;;      <groupId>com.rabbitmq</groupId>
;;      <artifactId>amqp-client</artifactId>
;;      <version>2.3.1</version>
;;    </dependency>


;; First we have to import the classes from the java library.
(import (com.rabbitmq.client ConnectionFactory Connection Channel QueueingConsumer))

;; And then we have to translate the equivalent java hello world program using
;; Clojure's excellent interop.

;; It feels very strange writing this sort of ceremony-oriented imperative code
;; in Clojure:

;; Make a connection factory on the local host
(def connection-factory
     (doto (ConnectionFactory.)
       (.setHost "localhost")))

;; and get it to make you a connection
(def connection (.newConnection connection-factory))

;; get that to make you a channel
(def channel (. connection createChannel))

;; And on that channel, create (or at least ensure the existence of) a queue,
;; named "hello", which is neither durable nor exclusive nor auto-deleted:
(. channel queueDeclare "hello" false false false nil)

;; Now we'll send ten messages to that queue:
(dotimes [ i 10 ]
  (. channel basicPublish "" "hello" nil (. (format "Hello World! (%d)" i) getBytes)))

;; Strictly we're sending the messages to the default exchange "", using the
;; routing key "hello"

;; Now we want to retrieve our messages from the queue.

;; We create a consumer
(def consumer (new QueueingConsumer channel))

;; And attach it to the channel
(. channel basicConsume "hello" true consumer)

;; consumer.nextDelivery() will block waiting for a message to arrive in the
;; queue, so we can just keep looping
(loop []
  (let [delivery (. consumer nextDelivery)
        str (String. (. delivery getBody))]
          (println str)
          (recur)))

;; The fun part is that we could run several copies of this program simultaneously

;; If you're using the maven-clojure-plugin, then the command to run this script
;; from the command line is:

;; $ mvn -Dclojure.script=rabbitmq.clj clojure:run

;; So you can run it on various seperate terminals at once.

;; The first one you run will just send messages to itself and print them out.
;; Later copies will send messages over which earlier ones will fight. It's most entertaining!

