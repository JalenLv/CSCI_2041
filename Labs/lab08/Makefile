.ONESHELL:

test: compile-test repl-test

compile-test: lab8.ml
	cat > lab8_compile_test.ml <<'EOF'
	open Lab8
	let expect_string expected actual =
	  if actual <> expected then failwith "unexpected string result"
	
	let expect_no_such_key thunk =
	  try
	    let _ = thunk () in
	    failwith "expected NoSuchKey"
	  with
	  | Association.NoSuchKey -> ()
	
	let () =
	  let a = Association.make () in
	  (match a with
	   | Association.Empty -> ()
	   | Association.Pair (_, _, _) -> failwith "make should return Empty");
	  let a0 =
	    Association.put 1 "I"
	      (Association.put 2 "II"
	        (Association.put 3 "III"
	          (Association.put 4 "IIII"
	            (Association.put 5 "V" a)))) in
	  expect_no_such_key (fun () -> Association.get a0 0);
	  expect_string "I" (Association.get a0 1);
	  expect_string "IIII" (Association.get a0 4);
	  let a1 = Association.put 4 "IV" a0 in
	  expect_string "IV" (Association.get a1 4);
	  let a2 = Association.delete a1 4 in
	  expect_string "IIII" (Association.get a2 4);
	  let a3 = Association.delete a1 5 in
	  expect_no_such_key (fun () -> Association.get a3 5)
	EOF
	ocamlc -o lab8_compile_test lab8.ml lab8_compile_test.ml
	./lab8_compile_test
	rm -f lab8_compile_test lab8_compile_test.ml lab8_compile_test.cmi lab8_compile_test.cmo lab8.cmi lab8.cmo

repl-test: lab8.ml tests8.ml
	echo '#use "lab8.ml" ;; #use "tests8.ml" ;;' | ocaml

zip:
	zip lab08 lab8.ml tests8.ml
