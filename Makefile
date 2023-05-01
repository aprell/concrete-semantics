test:
	@for N in 02; do \
	    if dune exec ./chapter$$N.exe; then \
	        printf "Chapter %s: \e[32mOK\e[0m\n" $$N; \
	    else \
	        printf "Chapter %s: \e[31mNG\e[0m\n" $$N; \
	    fi \
	done

clean:
	dune clean

.PHONY: clean test
