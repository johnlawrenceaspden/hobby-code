#!/usr/bin/env python

# http://www.rabbitmq.com/tutorial-one-python.html

# sudo apt-get install rabbitmq-server
# careful, commands below, to install pika, makes a subdirectory src/pika
# without asking
# sudo apt-get install python-pip git-core
# sudo pip install -e git+http://github.com/tonyg/pika.git#egg=pika

import pika

connection = pika.AsyncoreConnection(pika.ConnectionParameters('localhost'))
channel = connection.channel()


channel.queue_declare(queue='hello')

channel.basic_publish(exchange='',
                      routing_key='hello',
                      body='Hello World!')
print " [x] Sent 'Hello World'"

connection.close()

print "try $ sudo rabbitmqctl list_queues"
