CHAPTERS := 2 3 7 8 9

test:
	@./test.sh $(CHAPTERS)

clean:
	dune clean

.PHONY: clean test
