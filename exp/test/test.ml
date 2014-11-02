open OUnit2
open Exp

(* Funcao auxiliar para valores option *)

let valor_option v = 
  match v with
  | Some x -> x
  | None -> failwith "Valor esperado"


(* Algumas expressoes *)

(* 4 + 3 * 2 *)
let e1 = Soma (Const 4, Mult (Const 3, Const 2)) 

(* (4 + 3) * 2 *)
let e2 = Mult (Soma (Const 4, Const 3), Const 2) 

(* (4 + 3) * 2 + 5 *)
let e3 = Soma (Mult (Soma (Const 4, Const 3), Const 2), Const 5)

(* (11 + 0) * (9 - 2) *)
let e4 = Mult (Soma (Const 11, Const 0), Sub (Const 9, Const 2))


(* Testes do interpretador *)

let t_eval ctxt = 
  let pares = [(e1, 10); (e2, 14); (e3, 19); (e4, 77)] in
  List.iter (fun (e, v) -> assert_equal (eval e) v) pares


(* Programas para a maquina de pilha *)

let p1 = [Empilha 5; Empilha 3; Oper OpSoma]

let p2 = [Empilha 11; Empilha 0; Oper OpSoma; Empilha 2; Empilha 9; Oper OpSub; Oper OpMult]


(* Testes da maquina de pilha *)

let t_exec ctxt = 
  let pares = [(p1, Some 8); (p2, Some 77)] in
  List.iter (fun (p, v) -> assert_equal (executa p) v) pares


(* Testes do compilador *)

let t_compila ctxt = 
  assert_equal (compila e4) p2

let t_compila_executa ctxt = 
  let comp_exec e = 
    e |> compila |> executa |> valor_option 
  in 
  let pares = [(e1, 10); (e2, 14); (e3, 19); (e4, 77)] in
  List.iter (fun (e, v) -> assert_equal (comp_exec e) v) pares

(* a suite de testes *)
let suite = 
  "bateria01" >:::
    [
      "interpretador" >:: t_eval;
      "maquina de pilha" >:: t_exec;
      "compilador" >:: t_compila;
      "compilacao e execucao do programa" >:: t_compila_executa
    ]

(* executor dos testes *)
let () = 
  run_test_tt_main suite

