# 2012 (C) Jussi Rintanen, jrintanen.jr@gmail.com

# Uncomment for Madagascar with the planning heuristic as default (Mp)
VERSION = -DMPDOWNLOAD
EXECUTABLE=Mp
# Uncomment for Madagascar with VSIDS as default (M)
#VERSION = -DVSIDS
#EXECUTABLE=M

#INSTRUMENT = -g -ggdb

CONFIGURATION= -DLBD -DREPRTHREE -DWEIGHTS #-DMULTICORE #-DSPREAD -DCOSTS -DCFMA

#FEATURES= -fopenmp
#LINKFEATURES= -fopenmp

GCCOPTIMIZE= -O3 -fprefetch-loop-arrays -funroll-loops -ftree-loop-im -ftracer -maccumulate-outgoing-args -momit-leaf-frame-pointer #-falign-functions=64

CFLAGS = $(VERSION) $(FEATURES) $(CONFIGURATION) $(INSTRUMENT) $(GCCOPTIMIZE) -Wall

PARSER=y
CC = gcc

LINKFLAGS = -static
#LINKFLAGS = -s

AMD64FLAGS = #-march=corei7-avx -mcmodel=medium -mtune=corei7-avx

OBJ = $(PARSER).tab.o main.o asyntax.o lex.yy.o tables.o operators.o invariants.o intsets.o cleanup.o translate2sat.o scc.o clausesets.o printplan.o clausedb.o dimacsinput.o ground.o
HDR = main.h asyntax.h tables.h operators.h invariants.h intsets.h cleanup.h scc.h intsets.h translate2sat.h ordintsets.h clausedb.h interface.h printplan.h dimacsinput.h

all: nplan

nplan: date lex.yy.c Makefile $(PARSER).tab.c $(OBJ) $(HDR)
	$(CC) $(LINKFLAGS) $(LINKFEATURES) $(INSTRUMENT) $(OBJ) -lm -o $(EXECUTABLE)

clausesets.o: heuristics2.c varvals.c learn2.c

ground.o: Cground.c

%.o: %.c Makefile $(HDR)
	$(CC) $(CFLAGS) $(AMD64FLAGS) -c $<

date:
	./makedate

main.o: zPRINTDATE

y.tab.c: parser.y
	yacc -d parser.y

y.tab.h: parser.y
	yacc -d parser.y

lex.yy.c: lexer.lex $(PARSER).tab.h y.tab.h
	flex lexer.lex

clean:
	rm -f $(OBJ) y.tab.c y.tab.h

tar:
	tar cvf MADAGASCAR.TAR README asyntax.c asyntax.h clausedb.c clausedb.h clausesets.c clausesets.h cleanup.c cleanup.h dimacs.h dimacsinput.c dimacsinput.h ground.c Cground.c heuristics2.c interface.h intsets.c intsets.h invariants.c invariants.h lexer.lex main.c main.h makedate Makefile operators.c operators.h ordintsets.c ordintsets.h parser.y printplan.c printplan.h scc.c scc.h tables.c tables.h translate2sat.c translate2sat.h varvals.c learn2.c shortcuts.c zPOSTF zPREF
