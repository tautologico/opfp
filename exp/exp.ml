(* 

  Linguagem de expressoes aritmeticas
  Interpretador, compilador para maquina de pilha

 *)

type exp = 
    Const of int 
  | Adic of exp * exp 
  | Sub of exp * exp
  | Mult of exp * exp


