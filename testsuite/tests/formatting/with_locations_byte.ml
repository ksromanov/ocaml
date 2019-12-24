(* TEST
ocamlc_flags="-g -dlocations -dsource -dparsetree -dtypedtree -dlambda"
compile_only="true"
* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output
*)
let rec fib = function
  | 0 | 1 -> 1
  | n -> fib (n - 1) + fib (n - 2)
;;
