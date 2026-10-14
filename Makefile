BIN_DIR = _build/default/bin
EXEC = main.exe
TARGET = safe-m4

.PHONY: all build test clean install

all: test build

build:
	dune build --profile release

test:
	dune runtest

clean:
	dune clean

install: build
	install -Dm755 $(BIN_DIR)/$(EXEC) $(DESTDIR)/usr/bin/$(TARGET)
