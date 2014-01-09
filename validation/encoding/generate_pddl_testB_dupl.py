'''
Created on May 24, 2013

@author: jacopo
'''
import sys
import random
import math

delete_arcs = 0.2

def printHeader(stream,numComponents,numNodes):
    stream.write("(define (problem TestB" + str(numComponents) + "client_server)\n")
    stream.write("(:domain aeolus)\n")

def printObjects(stream,numComponents,numNodes):
    stream.write("(:objects\n")
    for i in range(0,numNodes):
        stream.write("\tnode" + str(i) + " - node\n")
    for i in range(0,1):
        for j in range(0,2):
            stream.write("\tres_" + str(i) + "_" + str(j) + " - resource\n")
    for i in range(1,numComponents):
        for j in range(0,3):
            stream.write("\tres_" + str(i) + "_" + str(j) + " - resource\n")
    for i in range(0,numComponents):
        for j in range(1,3):
            stream.write("\tport_" + str(i) + "_" + str(j) + " - port\n")
    stream.write(")\n")
    stream.flush()
    
def printInit(stream,numComponents,numNodes,sample):
    stream.write("(:init\n")
    for i in range(0,numComponents):
        stream.write("\t(initial_resource res_" + str(i) + "_0)\n")

    for i in range(0,1):
        stream.write("\t(transition res_" + str(i) + "_0 res_" + str(i) + "_1)\n")
    for i in range(1,numComponents):
        stream.write("\t(transition res_" + str(i) + "_0 res_" + str(i) + "_1)\n")
        stream.write("\t(transition res_" + str(i) + "_1 res_" + str(i) + "_2)\n")
    
    stream.write("\t(resource_provides_port res_0_1 port_0_2)\n")    
    for i in range(1,numComponents):
        stream.write("\t(resource_provides_port res_" + str(i) + "_1 port_" + str(i) + "_1)\n")
        stream.write("\t(resource_provides_port res_" + str(i) + "_2 port_" + str(i) + "_2)\n")
    for i in range(1,numComponents):
        if i not in sample:
            stream.write("\t(resource_provides_port res_" + str(i) + "_2 port_" + str(i) + "_1)\n")

    stream.write("\t(resource_requires_port res_0_1 port_1_1)\n")
    stream.write("\t(resource_requires_port res_" + str(numComponents-1) + "_2 port_" + str(numComponents-2) + "_2)\n")
    for i in range(1,numComponents-1):
        stream.write("\t(resource_requires_port res_" + str(i) + "_1 port_" + str(i+1) + "_1)\n")
        stream.write("\t(resource_requires_port res_" + str(i) + "_2 port_" + str(i+1) + "_1)\n")
        stream.write("\t(resource_requires_port res_" + str(i) + "_2 port_" + str(i-1) + "_2)\n")

    stream.write(")\n")       
    stream.flush()
    
def printGoal(stream,numComponents,numNodes):
    stream.write("(:goal\n")
    stream.write("\t(node_resource node0 res_" + str(numComponents-1) + "_2)\n")
    stream.write(")\n")
    stream.flush()
    
def printEnd(stream):
    stream.write(")\n")
    stream.flush() 

def main(args):
    numComponents = int(args[0])

    random.seed(1)
    sample = random.sample(range(1,numComponents), int(math.ceil(numComponents*delete_arcs)))

    numNodes = numComponents + len(sample)
    
    stream = sys.stdout
    printHeader(stream,numComponents,numNodes)
    printObjects(stream,numComponents,numNodes)
    printInit(stream,numComponents,numNodes,sample)
    printGoal(stream,numComponents,numNodes)
    printEnd(stream)

if __name__ == '__main__':
    main(sys.argv[1:])
