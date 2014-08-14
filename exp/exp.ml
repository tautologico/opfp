(* 

  Linguagem de expressoes aritmeticas
  Interpretador, compilador para maquina de pilha

 *)

(** {1 Expressões Aritméticas } *)

(** Tipo para expressões *)
type exp = 
  | Const of int 
  | Soma of exp * exp 
  | Sub of exp * exp
  | Mult of exp * exp

let rec print e = 
  match e with 
    Const n -> string_of_int n
  | Soma (e1, e2) -> Printf.sprintf "(%s + %s)" (print e1) (print e2)
  | Sub (e1, e2) -> Printf.sprintf "(%s - %s)" (print e1) (print e2)
  | Mult (e1, e2) -> Printf.sprintf "(%s * %s)" (print e1) (print e2)

(** Interpretador para expressões *)
let rec eval e = 
  match e with
    Const n -> n
  | Soma (e1, e2) -> eval e1 + eval e2
  | Sub (e1, e2) -> eval e1 - eval e2
  | Mult (e1, e2) -> eval e1 * eval e2

(** {1 Máquina de Pilha } *)

(** Operações da máquina *)
type operacao = OpSoma | OpSub | OpMult

(** Instruções da máquina *)
type instrucao = 
  | EmpConst of int
  | Oper of operacao

(** Um programa é uma lista de instrucoes *)
type programa = instrucao list 

(** A pilha da máquina é uma lista de valores inteiros *)
type pilha = int list 

(** Obtém dois operandos de uma pilha *)
let operandos p = 
  match p with
  | [] -> None
  | _ :: [] -> None
  | n1 :: n2 :: r -> Some ((n1, n2), r)

(** Obtem a função em OCaml que corresponde a cada operador da máquina *)
let oper o = 
  match o with
  | OpSoma -> (+)
  | OpSub -> (-)
  | OpMult -> ( * )  (* espaços são necessários para não confundir com comentário *)

(** Executa uma instrução da máquina de pilha. 
    Dada uma pilha, retorna a pilha resultante apos a execução. *)
let exec_inst p inst = 
  match inst with
    EmpConst n -> n :: p
  | Oper o -> 
     match operandos p with
       None -> p   (* mantem a mesma pilha se alguma operacao deu errado *)
     | Some ((n1, n2), r) -> 
        let op = oper o in
        (op n1 n2) :: r

(** Executa um programa da máquina de pilha, assumindo que a pilha 
    inicia vazia. *)
let rec exec_prog p = 
  List.fold_left exec_inst [] p 

(* TODO compilacao de expressoes para maquina de pilha *)  

(* TODO otimizacao *)

(* TODO traducao para ILasm *)
