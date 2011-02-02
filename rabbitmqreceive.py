#!/usr/bin/env python

# see rabbitmqsend.py for setup

import pika

connection = pika.AsyncoreConnection(pika.ConnectionParameters(host='localhost'))

channel = connection.channel()

channel.queue_declare(queue='hello')

def callback(ch, method, properties, body):
    print " [x] Received %r" % (body,)

channel.basic_consume(callback,
                      queue='hello',
                      no_ack=True)

print '[*] Waiting for messages. To exit press Ctrl+C'
pika.asyncore_loop()
