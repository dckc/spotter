main.byte: main.ml spotter.ml
	ocamlbuild -use-ocamlfind -pkgs camomile,zarith $@
