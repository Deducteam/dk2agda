test_run: dk2agda
	./dk2agda.sh -f test_files/sttfa/sttfa.dk -o out

.PHONY: dk2agda
dk2agda: dk2agda.ml
	ocamlfind ocamlopt -linkpkg -package lambdapi.core -package str $< -o $@

.PHONY: clean
clean:
	rm -rf dk2agda *.cmi *.cmx *.o

#./dk2agda.sh -f test_files/tests/OK/rewrite_pattern.lp -o out
#./dk2agda.sh -d test_files/tests/OK/ -o out
