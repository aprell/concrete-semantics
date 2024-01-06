CHAPTERS := 2 3 7 8 9 10 13

default: test-chapters

all test-all: test-chapters test-parse

test-chapters:
	@./test.sh $(CHAPTERS)

test-parse:
	dune exec ./test_parse.exe

clean:
	dune clean

.PHONY: all clean default
.PHONY: test-all test-chapters test-parse
