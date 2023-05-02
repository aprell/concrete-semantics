CHAPTERS := 2 3

test:
	@./test.sh $(CHAPTERS)

clean:
	dune clean

.PHONY: clean test
