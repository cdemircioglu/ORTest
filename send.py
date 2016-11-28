#!/usr/bin/env python
import pika
import sys

print sys.argv[1]

credentials = pika.PlainCredentials('controller', 'KaraburunCe2')
parameters = pika.ConnectionParameters('hwcontrol.cloudapp.net',5672,'/',credentials)
connection = pika.BlockingConnection(parameters)
channel = connection.channel()

channel.queue_declare(queue='workorder')

channel.basic_publish(exchange='',routing_key='workorder',body=sys.argv[1])


print(" [x] Sent message!")
connection.close()

