open OUnit2
open Exp

(* Algumas expressoes *)

(* 4 + 3 * 2 *)
let e1 = Soma (Const 4, Mult (Const 3, Const 2)) 

(* (4 + 3) * 2 *)
let e2 = Mult (Soma (Const 4, Const 3), Const 2) 

(* (4 + 3) * 2 + 5 *)
let e3 = Soma (Mult (Soma (Const 4, Const 3), Const 2), Const 5)


(* Testes do interpretador *)

let t_eval ctxt = 
  let pares = [(e1, 10); (e2, 14); (e3, 19)] in
  List.iter (fun (e, v) -> assert_equal (eval e) v) pares

(* a suite de testes *)
let suite = 
  "suite" >:::
    [
      "interpretador" >:: t_eval
    ]

(* executor dos testes *)
let () = 
  run_test_tt_main suite
