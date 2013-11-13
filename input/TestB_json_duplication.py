'''
TestB_json_duplication.py [Options]

Options:
    -s, --size=SIZE: number of components. Must be > 2 (default 5)
    -o, --output=FILE: file where to save the program (default stdoutput)

@author: jacopo
'''
import sys, getopt
import random
import math

delete_arcs = 0.2

def printFirst(stream,numComp):
    stream.write('\t{\n')
    stream.write('\t\t"u_cname" : "C0",\n')
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
    stream.write('\t\t\t\t\t"u_successors" : [],\n')
    stream.write('\t\t\t\t\t"u_provides" : ["p0_2"],\n')
    stream.write('\t\t\t\t\t"u_requires" : ["p1_1"]\n')
    stream.write('\t\t\t\t}\n')
          
   	# stream.write('\t\t\t\t{\n') 
    # stream.write('\t\t\t\t\t"id" : 2,\n')
    # stream.write('\t\t\t\t\t"successors" : [],\n')
    # stream.write('\t\t\t\t\t"provides" : ["p0_2"],\n')
    # stream.write('\t\t\t\t\t"requires" : []\n')
    # stream.write('\t\t\t\t}\n')
    
    stream.write('\t\t\t]\n') 
    stream.write('\t},\n')

    
def printLast(stream,numComp):
    stream.write('\t{\n')
    stream.write('\t\t"u_cname" : "C' + str(numComp-1) + '",\n')
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
    stream.write('\t\t\t\t\t"u_provides" : ["p' + str(numComp-1) + '_1"],\n')
    stream.write('\t\t\t\t\t"u_requires" : []\n')
    stream.write('\t\t\t\t},\n')
          
    stream.write('\t\t\t\t{\n') 
    stream.write('\t\t\t\t\t"u_name" : "s2",\n')
    stream.write('\t\t\t\t\t"u_successors" : [],\n')
    stream.write('\t\t\t\t\t"u_provides" : ["p' + str(numComp-1) + '_1"],\n')
    stream.write('\t\t\t\t\t"u_requires" : ["p' + str(numComp-2) + '_2"]\n')
    stream.write('\t\t\t\t}\n')

    stream.write('\t\t\t]\n') 
    stream.write('\t}\n')

def printMiddle(stream,num, sample):
    stream.write('\t{\n')
    stream.write('\t\t"u_cname" : "C' + str(num) + '",\n')
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
    stream.write('\t\t\t\t\t"u_provides" : ["p' + str(num) + '_1"],\n')
    stream.write('\t\t\t\t\t"u_requires" : ["p' + str(num+1) + '_1"]\n')
    stream.write('\t\t\t\t},\n')
          
    stream.write('\t\t\t\t{\n') 
    stream.write('\t\t\t\t\t"u_name" : "s2",\n')
    stream.write('\t\t\t\t\t"u_successors" : [],\n')
    if num in sample:
        stream.write('\t\t\t\t\t"u_provides" : ["p' + str(num) + '_2"],\n')
    else:
        stream.write('\t\t\t\t\t"u_provides" : ["p' + str(num) + '_2", "p' + str(num) + '_1"],\n')
    stream.write('\t\t\t\t\t"u_requires" : ["p' + str(num-1) + '_2" , "p' + str(num+1) + '_1"]\n')
    stream.write('\t\t\t\t}\n')

    stream.write('\t\t\t]\n') 
    stream.write('\t},\n')

def main():
    try:
        opts, args = getopt.getopt(sys.argv[1:], "o:s:h", ["output=", "size=", "help"])
    except getopt.GetoptError as err:
        # print help information and exit:
        print str(err) # will print something like "option -a not recognized"
        print __doc__
        sys.exit(2)
    
    stream = sys.stdout
    numComp = 5
    random.seed(1)
    fileOpt = False
    for o, a in opts:
        if o in ("-s", "--size"):
            numComp = int(a)
            assert(numComp > 2)
        elif o in ("-h", "--help"):
            print __doc__
            sys.exit()
        elif o in ("-o", "--output"):
            fileOpt = True
            stream = open(a,'w')
        else:
            assert False, "unhandled option"
    
    #stream.write('"universe" :\n');
    
    sample = random.sample(range(1,numComp), int(math.ceil(numComp*delete_arcs)))
    #stream.write(str(sample));
    stream.write("[\n");
    printFirst(stream,numComp)
    for i in range(1,numComp-1):
        printMiddle(stream,i,sample)
    printLast(stream, numComp)
    
    stream.write("]\n");
    stream.flush()
    if fileOpt:
        stream.close()

if __name__ == '__main__':
    main()

