ILASM=ilasm
BIN=blaise


all: $(BIN).exe
	mono $(BIN).exe
	rm Runtime.dll Record.dll Array.dll

$(BIN).exe: $(BIN).il Runtime.dll Record.dll Array.dll
	$(ILASM) $(BIN).il

Runtime.dll: Runtime.cs
	mcs /target:library Runtime.cs

Record.dll: Record.cs
	mcs /target:library Record.cs

Array.dll: Array.cs
	mcs /target:library Array.cs

clean: 
	rm Runtime.dll Record.dll Array.dll $(BIN).exe
