test_run: dk2agda
	./$< test_files/main.lp out

.PHONY: dk2agda
dk2agda: dk2agda.ml
	ocamlfind ocamlopt -linkpkg -package lambdapi.core $< -o $@

.PHONY: clean
clean:
	rm -rf dk2agda *.cmi *.cmx *.o
