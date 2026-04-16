test: lab9.ml tests9.ml
	sed '/^type *$$/,/Symbol of string ;;/d' tests9.ml > tests9.stripped.ml
	echo '#use "lab9.ml" ;; #use "tests9.stripped.ml" ;;' | ocaml
	rm -f tests9.stripped.ml

zip:
	zip lab09 lab9.ml tests9.ml
