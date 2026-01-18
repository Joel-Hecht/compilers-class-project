:SRC := $(wildcard src/*.janet)
TEST := $(wildcard test/*.janet)

.PHONY: all
all: $(SRC)
	jpm -l deps
	jpm build

.PHONY: test
test: $(SRC) $(TEST)
	jpm test

.PHONY: clean
clean: 
	rm -rf build
	
