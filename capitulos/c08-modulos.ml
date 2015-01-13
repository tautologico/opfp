(* 

  OCaml: Programação Funcional na Prática
  Andrei de A. Formiga - Casa do Código

  Exemplos do Capítulo 08 - Módulos

 *)

(* 
  Os exemplos deste capítulo foram pensados para uso no REPL, digitando uma 
  expressão de cada vez. Quem estiver usando um editor integrado com o REPL 
  pode selecionar cada expressão neste arquivo e mandar para o REPL. 

  Para deixar claro onde termina cada expressão, e para ficar mais fácil 
  de copiar e colar cada uma no REPL, as expressões no arquivo terminam com ;;,
  apesar de código OCaml em arquivos não precisar deste terminador. 
*)

(** 8.1 Estruturas e assinaturas *)


module Mod = 
  struct 
    let x = 0
  end;;

module type Mod_sig = 
  sig 
    val x : int
  end;;

module Mod : Mod_sig = 
  struct
    let x = 0
  end;;


(** 8.2 Acesso aos itens de um módulo *)

Mod.x;;

module Mod2 : Mod_sig = 
  struct
    let y = 6
    let x = y * 7
  end;;

Mod2.y;;

module Ponto2D = 
struct
  type t = { x : int; y : int }

  let zero () = { x = 0; y = 0 }
end

let p1 = Ponto2D.zero ();;

p1.Ponto2D.x;;

p1.x;;

(* Tipos abstratos *)

type estado_arq = Fechado | Aberto

type arq = { nome : string; estado : estado_arq }

let a = { nome = "arquivo.txt"; estado = Aberto } ;;

module type Arq_sig = 
  sig
    type t
  end;;

module Arq : Arq_sig = 
  struct
    type estado = Fechado | Aberto
    type t = { nome: string; estado: estado }
  end;;

module type Arq_sig = 
sig 
  type t
  val criar : string -> t
  val abrir : t -> unit
end

module Arq : Arq_sig = 
struct
  type estado = Fechado | Aberto
  type t = { nome: string; mutable estado: estado }
  let criar nome = { nome; estado = Fechado }
  let abrir arq = arq.estado <- Aberto
end

let a1 = Arq.criar "arquivo.txt";;

Arq.abrir a1;;

(* Abrindo um módulos *)

open Arq;;

let a1 = criar "arquivo.txt";;

let a1 = let open Arq in criar "arquivo.txt";;

let a2 = Arq.(criar "arquivo.txt");;

module Ponto2D = 
struct
  type t = { x : int; y : int }

  let zero () = { x = 0; y = 0 }

  let criar x y = { x; y }

  let ( + ) p1 p2 = { x = p1.x + p2.x; y = p1.y + p2.y }
end

let mult_coords p = Ponto2D.(p.x * p.y)

let p1 = let open Ponto2D in
         (criar 3 4) + (criar 11 8);;

(** 8.3 Módulos e arquivos *)


(* 
  O exemplo desta seção envolve o arquivo ponto2D.ml, 
  que define o módulo Ponto2D.
  https://github.com/tautologico/opfp/blob/master/capitulos/ponto2D.ml
*)

(* Execução de código e o ponto de entrada *)

(* 
  Nesta seção é mencionado o arquivo arvgenero.ml do capítulo 09,
  este arquivo se encontra no diretório titanic do mesmo repositório.
  https://github.com/tautologico/opfp/blob/master/titanic/src/arvgenero.ml
*)

(** 8.4 Funtores *)

module type Arq = 
sig
  type t

  val abrir : string -> t

  val fechar : t -> unit
end

module Log = 
struct
  let registra str = Printf.printf "::: LOG: %s\n" str
end

module ArqLog(A : Arq) : Arq = 
  struct
    type t = A.t

    let abrir nome = 
      let logstr = Printf.sprintf "abrindo arquivo %s" nome in
      Log.registra logstr; 
      A.abrir nome

    let fechar arq = 
      Log.registra "fechando arquivo";
      A.fechar arq
  end;;

module ArqLeitura : Arq = 
  struct
    type t = { nome: string; ch: in_channel }
               
    let abrir nome = 
      { nome; ch = open_in nome }
        
    let fechar t = close_in t.ch
  end

module ArqLerLog = ArqLog(ArqLeitura);;

(* Conjuntos e mapas *)

module Set = 
struct
  type 'a t = No of 'a * 'a t * 'a t | Folha

  let rec busca s x = 
    match s with
    | No (k, esq, dir) when k = x -> Some s
    | No (k, esq, dir) ->  
        if x < k then busca esq x 
        else busca dir x
    | Folha -> None
end

[1; 2; 3] < [1; 2; 3; 4];;

[| 2; 1; 1 |] < [| 1; 7; 9 |];;

module type Comp = 
sig
  type t

  val compare : t -> t -> int
end

module Set(E : Comp) = 
struct 
  type t = No of E.t * t * t | Folha

  let vazio = Folha

  let rec busca s x = 
    match s with
    | No (k, esq, dir) when E.compare k x = 0 -> Some s
    | No (k, esq, dir) ->  
        if E.compare k x < 0 then busca esq x 
        else busca dir x
    | Folha -> None

  let rec adiciona s x = 
    match s with
    | No (k, esq, dir) when E.compare k x = 0 -> s
    | No (k, esq, dir) ->  
        if E.compare k x < 0 then No (k, adiciona esq x, dir)
        else No (k, esq, adiciona dir x)
    | Folha -> No (x, Folha, Folha)

  let rec lista s = 
    match s with
    | No (k, esq, dir) -> k :: (lista esq @ lista dir)
    | Folha -> []
end

module IntComp : Comp = 
struct
  type t = int
  let compare = compare
end

module IntSet = Set(IntComp)


module IntSet = Set(struct 
                      type t = int 
                      let compare = compare 
                    end)

let remove_dup l = 
  let conj = List.fold_left IntSet.adiciona IntSet.vazio l in
  IntSet.lista conj

remove_dup [1; 3; 1; 4; 5; 4; 2];;

(* Módulos locais *)

let remove_dup l = 
  let module ISet = 
    Set(struct type t = int let compare = compare end) 
  in
  let conj = List.fold_left ISet.adiciona ISet.vazio l in
  ISet.lista conj

(* Restrições de compartilhamento *)

module IntComp : Comp = 
struct
  type t = int
  let compare = compare
end

module IntSet = Set(IntComp)

let remove_dup l = 
  let conj = List.fold_left IntSet.adiciona IntSet.vazio l in
  IntSet.lista conj

(* O código a seguir não compila: *)
(*
# remove_dup [1; 2; 1; 8; 7; 2];;
Error: This expression has type int but an expression 
was expected of type IntComp.t
 *)

module IntComp : Comp with type t = int = 
struct
  type t = int
  let compare = compare
end

module IntSet = Set(IntComp)


module IntComp : Comp with type t = int = 
struct type t = int let compare = compare end;;

module IntComp : Comp with type t := int = 
struct type t = int let compare = compare end;;

(* Conjuntos e mapas na biblioteca padrão *)

module IntSet = 
  Set.Make(struct type t = int let compare = compare end)


(** 8.5 Extensão de estruturas e assinaturas *)

module type Comp = 
sig
  type t

  val compare : t -> t -> int
end

module type CompHash = 
sig
  include Comp

  val hash : t -> int
end

module type CompHash = 
sig
  type t

  val compare : t -> t -> int

  val hash : t -> int
end

module IntComp : Comp with type t = int = 
struct
  type t = int
  let compare = compare
end

module IntCompHash : CompHash with type t = int = 
struct
  include IntComp

  let hash n = n
end

(** 8.6 Módulos de primeira classe *)

(module IntComp : Comp);;

let m = (module IntComp : Comp);;

type comparador = { nome: string; modulo : (module Comp) };;

let comp1 = { nome = "comparador de inteiros"; modulo = m };;

module type Mensagem = sig 
  val msg : string
end

module Hello : Mensagem = struct 
  let msg = "Hello" 
end

module QuePasa : Mensagem = struct 
  let msg = "Que pasa?" 
end

let imprime_msg m = 
  let module M = (val m : Mensagem) in 
  print_endline M.msg

let mensagens : (module Mensagem) list = 
  [(module Hello : Mensagem); (module QuePasa : Mensagem)]

List.iter imprime_msg mensagens;;

