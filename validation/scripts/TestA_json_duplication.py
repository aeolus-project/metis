'''
TestA_json_duplication.py [Options]

Options:
    -s, --size=SIZE: number of states. Must be > 2 (default 5)
    -o, --output=FILE: file where to save the program (default stdoutput)

@author: jacopo
'''
import sys, getopt
import random
import math

delete_arcs = 0.2

def printFirst(stream,numStates,sample):
    stream.write('\t{\n')
    stream.write('\t\t"u_cname" : "A",\n')
    stream.write('\t\t"u_automaton" :\n') 
    stream.write('\t\t\t[\n') 
    
    stream.write('\t\t\t\t{\n') 
    stream.write('\t\t\t\t\t"u_name" : "s0",\n')
    stream.write('\t\t\t\t\t"u_successors" : ["s1"],\n')
    stream.write('\t\t\t\t\t"u_provides" : [],\n')
    stream.write('\t\t\t\t\t"u_requires" : []\n')
    stream.write('\t\t\t\t},\n')
    
    stream.write('\t\t\t\t{\n')
    stream.write('\t\t\t\t\t"u_name" : "s1",\n')
    stream.write('\t\t\t\t\t"u_successors" : ["s2"],\n')
    stream.write('\t\t\t\t\t"u_provides" : ["p0_1"],\n')
    stream.write('\t\t\t\t\t"u_requires" : []\n')
    stream.write('\t\t\t\t},\n')
    
    for i in range(2,numStates-1):
        stream.write('\t\t\t\t{\n') 
        stream.write('\t\t\t\t\t"u_name" : ' + '"s' + str(i) + '"' + ',\n')
        stream.write('\t\t\t\t\t"u_successors" : [' + '"s' + str(i+1) + '"'  + '],\n')
        if i in sample:
            stream.write('\t\t\t\t\t"u_provides" : ["p0_' + str(i) + '"],\n')
        else:
            stream.write('\t\t\t\t\t"u_provides" : ["p0_' + str(i) + '", "p0_' + str(i-1) + '"],\n')
        stream.write('\t\t\t\t\t"u_requires" : ["p1_' + str(i) + '"]\n')
        stream.write('\t\t\t\t},\n')
       
    stream.write('\t\t\t\t{\n') 
    stream.write('\t\t\t\t\t"u_name" : ' + '"s' + str(numStates-1) + '"' + ',\n')
    stream.write('\t\t\t\t\t"u_successors" : [],\n')
    stream.write('\t\t\t\t\t"u_provides" : ["p0_' + str(numStates-1) + '", "p0_' + str(numStates-2) + '"],\n')
    stream.write('\t\t\t\t\t"u_requires" : ["p1_' + str(numStates-1) + '"]\n')
    stream.write('\t\t\t\t}\n')

    stream.write('\t\t\t]\n') 
    stream.write('\t},\n')

    
def printSecond(stream,numStates):
    stream.write('\t{\n')
    stream.write('\t\t"u_cname" : "B",\n')
    stream.write('\t\t"u_automaton" :\n') 
    stream.write('\t\t\t[\n') 
    
    stream.write('\t\t\t\t{\n') 
    stream.write('\t\t\t\t\t"u_name" : "q0",\n')
    stream.write('\t\t\t\t\t"u_successors" : ["q1"],\n')
    stream.write('\t\t\t\t\t"u_provides" : [],\n')
    stream.write('\t\t\t\t\t"u_requires" : []\n')
    stream.write('\t\t\t\t},\n')
    
    stream.write('\t\t\t\t{\n') 
    stream.write('\t\t\t\t\t"u_name" : "q1",\n')
    stream.write('\t\t\t\t\t"u_successors" : ["q2"],\n')
    stream.write('\t\t\t\t\t"u_provides" : ["p1_2"],\n')
    stream.write('\t\t\t\t\t"u_requires" : ["p0_1"]\n')
    stream.write('\t\t\t\t},\n')
    
    for i in range(2,numStates-1):
        stream.write('\t\t\t\t{\n') 
        stream.write('\t\t\t\t\t"u_name" : ' + '"q' + str(i) + '"' + ',\n')
        stream.write('\t\t\t\t\t"u_successors" : [' + '"q'  + str(i+1) + '"'  + '],\n')
        stream.write('\t\t\t\t\t"u_provides" : ["p1_' + str(i) + '", "p1_' + str(i+1) + '"],\n')
        stream.write('\t\t\t\t\t"u_requires" : ["p0_' + str(i) + '"]\n')
        stream.write('\t\t\t\t},\n')
       
    stream.write('\t\t\t\t{\n') 
    stream.write('\t\t\t\t\t"u_name" : ' + '"q' + str(numStates-1) + '"' + ',\n')
    stream.write('\t\t\t\t\t"u_successors" : [],\n')
    stream.write('\t\t\t\t\t"u_provides" : ["p1_' + str(numStates-1) + '"],\n')
    stream.write('\t\t\t\t\t"u_requires" : ["p0_' + str(numStates-1) + '"]\n')
    stream.write('\t\t\t\t}\n')

    stream.write('\t\t\t]\n') 
    stream.write('\t}\n')


def main():
    try:
        opts, args = getopt.getopt(sys.argv[1:], "o:s:h", ["output=", "size=", "help"])
    except getopt.GetoptError as err:
        # print help information and exit:
        print str(err) # will print something like "option -a not recognized"
        print __doc__
        sys.exit(2)
    
    stream = sys.stdout
    numStates = 5
    random.seed(1)
    fileOpt = False
    for o, a in opts:
        if o in ("-s", "--size"):
            numStates = int(a)
            assert(numStates > 2)
        elif o in ("-h", "--help"):
            print __doc__
            sys.exit()
        elif o in ("-o", "--output"):
            fileOpt = True
            stream = open(a,'w')
        else:
            assert False, "unhandled option"
    
    sample = random.sample(range(1,numStates), int(math.ceil(numStates*delete_arcs)))
    #stream.write('"universe" :\n');
    stream.write("[\n");
    printFirst(stream,numStates,sample)
    printSecond(stream, numStates)
    
    stream.write("]\n");
    stream.flush()
    if fileOpt:
        stream.close()

if __name__ == '__main__':
    main()

