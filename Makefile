:SRC := $(wildcard src/*.janet)
TEST := $(wildcard test/*.janet)
SHELL := /bin/bash

.PHONY: all
all: $(SRC)
	jpm -l deps
	jpm -l build

.PHONY: test
test: $(SRC) $(TEST)
	jpm -l test
	./jpm_tree/bin/judge 

# Only use if you are adding new tests, or if a test has failed and you need to re-make the tests
.PHONY: judge
judge:
	./jpm_tree/bin/judge -i
	#./jpm_tree/bin/judge;read && ./scripts/mergeJudgeTests.sh

.PHONY: clean
clean: 
	rm -rf build
	
