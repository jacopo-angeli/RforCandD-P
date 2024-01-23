import queue
import random
import threading
from typing import List
from Message import MessageInterface
from Project.src.node import Node


from src import list_lock, node_list, thread_list


def append_to_list(value):
    with list_lock:
        node_list.append(value)


def newNode():
    # Crea una lista vuota di messaggi
    # Crea un nodo passando nodeList
    # Inserisce la pair {id del nodo, List} all'interno di nodeList
    thread_list.append(threading.Thread(target=Node(len(node_list), append_to_list)))
    node_list.append({len(node_list), queue.Queue[MessageInterface]()})
    thread_list[len(thread_list) - 1].run()
