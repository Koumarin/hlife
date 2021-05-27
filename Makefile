# -*- Makefile -*-
.POSIX:

HC      = ghc
HCFLAGS = -Wall

HS =	src/main.hs

BIN = hlife

all: $(BIN)

clean:
	rm -rf $(BIN)

run: $(BIN)
	./$(BIN)

$(BIN): $(HS)
	$(HC) -o $(BIN) $(HS)
