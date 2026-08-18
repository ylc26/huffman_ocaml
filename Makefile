OCAMLC ?= ocamlc
SOURCE := src/huffman.ml
TARGET := build/huffman

.PHONY: all run clean

all: $(TARGET)

$(TARGET): $(SOURCE)
	mkdir -p build
	$(OCAMLC) -o $(TARGET) $(SOURCE)

run: $(TARGET)
	mkdir -p output
	./$(TARGET)

clean:
	rm -rf build output
