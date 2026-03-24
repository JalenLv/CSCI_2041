(*
  Tests for Lab 8.
*)

(*
   YOUR CODE GOES HERE!
*)

(* Tests, worth 30 points. These tests aren't worth many points because all the
   code for this assignment is similar to code you've seen before. *)

let a = Association.make () ;;
(* val a : ('a, 'b) Association.t = Association.Empty               2 points *)

let a0 =
  Association.put 1 "I"
   (Association.put 2 "II"
     (Association.put 3 "III"
       (Association.put 4 "IIII"
         (Association.put 5 "V" a)))) ;;
(* val a0 : (int, string) Association.t =
     Association.Pair (1, "I",
      Association.Pair (2, "II",
       Association.Pair (3, "III",
        Association.Pair (4, "IIII",
         Association.Pair (5, "V", Association.Empty)))))           2 points *)

try
  Association.get a0 0
with
  Association.NoSuchKey -> "" ;;
(* - : string = ""                                                  2 points *)

Association.get a0 1 ;;
(* - : string = "I"                                                 2 points *)

Association.get a0 2 ;;
(* - : string = "II"                                                2 points *)

Association.get a0 3 ;;
(* - : string = "III"                                               2 points *)

Association.get a0 4 ;;
(* - : string = "IIII"                                              2 points *)

let a1 = Association.put 4 "IV" a0 ;;
(* val a1 : (int, string) Association.t =
     Association.Pair (4, "IV",
      Association.Pair (1, "I",
       Association.Pair (2, "II",
        Association.Pair (3, "III",
         Association.Pair (4, "IIII",
          Association.Pair (5, "V", Association.Empty))))))         2 points *)

Association.get a1 1 ;;
(* - : string = "I"                                                 2 points *)

Association.get a1 4 ;;
(* - : string = "IV"                                                2 points *)

let a2 = Association.delete a1 4 ;;
(* val a2 : (int, string) Association.t =
     Association.Pair (1, "I",
      Association.Pair (2, "II",
       Association.Pair (3, "III",
        Association.Pair (4, "IIII",
         Association.Pair (5, "V", Association.Empty)))))           2 points *)

Association.get a2 1 ;;
(* - : string = "I"                                                 2 points *)

Association.get a2 4 ;;
(* - : string = "IIII"                                              2 points *)

let a3 = Association.delete a1 5 ;;
(* val a3 : (int, string) Association.t =
     Association.Pair (4, "IV",
      Association.Pair (1, "I",
       Association.Pair (2, "II",
        Association.Pair (3, "III",
         Association.Pair (4, "IIII", Association.Empty)))))        2 points *)
try
  Association.get a3 5
with
  Association.NoSuchKey -> "" ;;
(* - : string = ""                                                  2 points *)
