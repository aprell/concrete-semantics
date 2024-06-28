CHAPTERS := $(sort $(wildcard chapter*.ml))

default: test-chapters

all test-all: test-chapters test-parse

test-chapters:
	@./test.sh $(CHAPTERS:.ml=.exe)

test-parse:
	dune exec ./test_parse.exe

clean:
	dune clean

.PHONY: all clean default
.PHONY: test-all test-chapters test-parse
