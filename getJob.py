#!/usr/bin/env python
import pika
import sys
import subprocess
import time
import os

print  sys.argv[1]

credentials = pika.PlainCredentials('controller', 'KaraburunCe2')
parameters = pika.ConnectionParameters('HWLinux',5672,'/',credentials)

connection = pika.BlockingConnection(parameters)
channel = connection.channel()

channel.queue_declare(queue=sys.argv[1])

def callback(ch, method, properties, body):
    print(" [x] Received %r" % body)
    time.sleep(5)
    print(" [x] Done")
    os.system("/usr/bin/Rscript --vanilla RWorker_test.R \"" +  body + "\"")
    ch.basic_ack(delivery_tag = method.delivery_tag)

channel.basic_qos(prefetch_count=1)
channel.basic_consume(callback,queue=sys.argv[1])
channel.start_consuming()

