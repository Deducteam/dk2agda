test_run: dk2agda
	./dk2agda.sh -d test_files/tests/OK -o out

.PHONY: dk2agda
dk2agda: dk2agda.ml
	ocamlfind ocamlopt -linkpkg -package lambdapi.core $< -o $@

.PHONY: clean
clean:
	rm -rf dk2agda *.cmi *.cmx *.o
