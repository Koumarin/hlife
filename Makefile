# -*- Makefile -*-
.POSIX:

HC      = ghc
HCFLAGS = -Wall

HS =	src/Main.hs		\
	src/Life.hs		\
	src/withTerminal.hs	\
	src/Util.hs

HSOBJ = $(HS:.hs=.o)
HSINT = $(HS:.hs=.hi)

BIN = hlife

all: $(BIN)

clean:
	rm -rf $(HSOBJ)
	rm -rf $(HSINT)
	rm -rf $(BIN)

run: $(BIN)
	./$(BIN)

$(BIN): $(HS)
	$(HC) $(HCFLAGS) -o $(BIN) $(HS)
