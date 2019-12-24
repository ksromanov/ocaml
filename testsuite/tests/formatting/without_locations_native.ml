(* TEST
flags="-g -dno-locations -dcmm -dsel -dlinear"
compile_only="true"
* setup-ocamlopt.byte-build-env
** ocamlopt.byte
*** check-ocamlopt.byte-output
*)
let rec fib = function
  | 0 | 1 -> 1
  | n -> fib (n - 1) + fib (n - 2)
;;
