.PHONY: test_run
test_run: dk2agda
	./dk2agda.sh -d test_files/tests/OK -o out

.PHONY: bench
bench: test_run
	(cd out ; find . -name "*.agda" -exec agda {} \;)
	ls out/*.agdai | wc -l
	ls out/*.agda | wc -l

dk2agda: dk2agda.ml
	ocamlfind ocamlopt -linkpkg -package lambdapi.core -package str $< -o $@

.PHONY: clean
clean:
	rm -f dk2agda *.cmi *.cmx *.o

.PHONY: fullclean
fullclean: clean
	rm -f out/*.agda out/*.agda out/.*.vim
