BIN=./bin
SRC=./src
LONG_SRC=$(SRC)/org/ucombinator/experimental
SCALA=scala
SCALA_FLAGS=-cp $(BIN)
SCALAC=scalac
SCALAC_FLAGS=-verbose -deprecation -feature -unchecked -d $(BIN)
TLC=org.ucombinator.experimental.Analyzer

all: bindir
	$(SCALAC) $(SCALAC_FLAGS) $(LONG_SRC)/*.scala
	
run: all
	$(SCALA) $(SCALA_FLAGS) $(TLC)

bindir:
	mkdir -p $(BIN)

clean:
	rm -rf $(BIN)/*
