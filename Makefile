ILASM=ilasm
BIN=blaise


all: $(BIN).exe
	mono $(BIN).exe

$(BIN).exe: $(BIN).il Runtime.dll
	$(ILASM) $(BIN).il

Runtime.dll: Runtime.cs
	mcs /target:library Runtime.cs

clean: 
	rm Runtime.dll $(BIN).exe
