'''
Created on May 24, 2013

@author: jacopo
'''
import sys, getopt
import random
import math

delete_arcs = 0.2

def printHeader(stream,numStates,numNodes):
    stream.write("(define (problem TestA" + str(numStates) + "client_server)\n")
    stream.write("(:domain aeolus)\n")

def printObjects(stream,numStates,numNodes):
    stream.write("(:objects\n")
    for i in range(0,numNodes):
        stream.write("\tnode" + str(i) + " - node\n")
    for i in range(0,2):
        for j in range(0,numStates):
            stream.write("\tres_" + str(i) + "_" + str(j) + " - resource\n")
    for i in range(0,2):
        for j in range(1,numStates):
            stream.write("\tport_" + str(i) + "_" + str(j) + " - port\n")
    stream.write(")\n")
    stream.flush()
    
def printInit(stream,numStates,numNodes,sample):
    stream.write("(:init\n")
    for i in range(0,2):
        stream.write("\t(initial_resource res_" + str(i) + "_0)\n")
        
    for i in range(0,2):
        for j in range(0,numStates-1):
            stream.write("\t(transition res_" + str(i) + "_" + str(j) + " res_" + str(i) + "_" + str(j+1) + ")\n")
    
    stream.write("\t(resource_provides_port res_0_" + str(numStates-1) + " port_0_" + str(numStates-1)+ ")\n")
    for i in range(2,numStates):
        if i not in sample:
            stream.write("\t(resource_provides_port res_0_" + str(i) + " port_0_" + str(i-1)+ ")\n")
        stream.write("\t(resource_provides_port res_0_" + str(i-1) + " port_0_" + str(i-1)+ ")\n")

    for i in range(2,numStates):
        stream.write("\t(resource_provides_port res_1_" + str(i) + " port_1_" + str(i)+ ")\n")
        stream.write("\t(resource_provides_port res_1_" + str(i-1) + " port_1_" + str(i)+ ")\n")
        
#     stream.write("\t(resource_requires_port res_0_1 port_1_1)\n")
#     stream.write("\t(resource_requires_port res_" + str(numStates-1) + "_2 port_" + str(numStates-2) + "_2)\n")
    for i in range(2,numStates):
        stream.write("\t(resource_requires_port res_0_" + str(i) + " port_1_"+ str(i) + ")\n")
    for i in range(1,numStates):
        stream.write("\t(resource_requires_port res_1_" + str(i) + " port_0_"+ str(i) + ")\n")
    
    stream.write(")\n")       
    stream.flush()
    
def printGoal(stream,numStates,numNodes):
    stream.write("(:goal\n")
    stream.write("\t(node_resource node0 res_1_" + str(numStates-1) + ")\n")
    stream.write(")\n")
    stream.flush()
    
def printEnd(stream):
    stream.write(")\n")
    stream.flush() 

def main(args):
    numStates = int(args[0])
    
    random.seed(1)
    sample = random.sample(range(2,numStates-1), int(math.ceil(numStates*delete_arcs)))

    numNodes = 2 + len(sample)

    stream = sys.stdout
    printHeader(stream,numStates,numNodes)
    printObjects(stream,numStates,numNodes)
    printInit(stream,numStates,numNodes,sample)
    printGoal(stream,numStates,numNodes)
    printEnd(stream)

if __name__ == '__main__':
    main(sys.argv[1:])

