(* 

  Linguagem de expressoes aritmeticas
  Interpretador, compilador para maquina de pilha

 *)

(** Expressoes aritmeticas *)
type exp = 
  | Const of int 
  | Adic of exp * exp 
  | Sub of exp * exp
  | Mult of exp * exp

(** Interpretador para expressoes *)
let rec eval e = 
  match e with
    Const n -> n
  | Adic (e1, e2) -> eval e1 + eval e2
  | Sub (e1, e2) -> eval e1 - eval e2
  | Mult (e1, e2) -> eval e1 * eval e2

(** Uma maquina de pilha para executar expressoes *)
type operacao = OpSoma | OpSub | OpMult

type instrucao = 
  | EmpConst of int
  | Oper of operacao

type programa = instrucao list 
type pilha = int list 

let operandos p = 
  match p with
  | [] -> None
  | _ :: [] -> None
  | n1 :: n2 :: r -> Some ((n1, n2), r)

let oper o = 
  match o with
  | OpSoma -> (+)
  | OpSub -> (-)
  | OpMult -> ( * )

(** Execucao de instrucoes da maquina de pilha *)
let exec_inst p inst = 
  match inst with
    EmpConst n -> n :: p
  | Oper o -> 
     match operandos p with
       None -> p   (* mantem a mesma pilha se alguma operacao deu errado *)
     | Some ((n1, n2), r) -> 
        let op = oper o in
        (op n1 n2) :: r

(** Execucao de programas da maquina de pilha *)
let rec exec_prog p = 
  List.fold_left exec_inst [] p 

(* TODO compilacao de expressoes para maquina de pilha *)  

(* TODO otimizacao *)

(* TODO traducao para ILasm *)
