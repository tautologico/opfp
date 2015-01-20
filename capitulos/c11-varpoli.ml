(* 

  OCaml: Programação Funcional na Prática
  Andrei de A. Formiga - Casa do Código

  Exemplos do Capítulo 11 - Variantes polimórficas e extensíveis

 *)

(* 
  Os exemplos deste capítulo foram pensados para uso no REPL, digitando uma 
  expressão de cada vez. Quem estiver usando um editor integrado com o REPL 
  pode selecionar cada expressão neste arquivo e mandar para o REPL. 

  Para deixar claro onde termina cada expressão, e para ficar mais fácil 
  de copiar e colar cada uma no REPL, as expressões no arquivo terminam com ;;,
  apesar de código OCaml em arquivos não precisar deste terminador. 
*)

(** 11.1 Limitações dos tipos variantes *)

(* Exemplo de JSON
{
    "url": "http://xkcd.com",
    "descricao": "Quadrinho XKCD",
    "acessos": 22,
    "ultimo": "2014-10-15 14:17:22"
}
 *)

type json = 
| String of string 
| Num of float
| Null 
| Bool of bool 
| Lista of json list 
| Obj of (string * json) list


(* Representação do exemplo de JSON usando o tipo criado *)
let hist1 = 
  Obj [("url", String "http://xkcd.com"); 
       ("descricao", String "Quadrinho XKCD");
       ("acessos", Num 22.0);
       ("ultimo", String "2014-10-15 14:17:22")]


module JSON = struct
  type t = 
  | String of string 
  | Num of float
  | Null 
  | Bool of bool 
  | Lista of t list 
  | Obj of (string * t) list
end

module BSON = struct
  type t = 
  | String of string 
  | Null 
  | Bool of bool 
  | Lista of t list 
  | Obj of (string * t) list
  | Data of int
end

(** 11.2 Variantes polimórficas *)

`Null;;

let valor_string v = 
  match v with
  | `Null -> "(null)"
  | `Num n -> string_of_float n
  | `String s -> s;;

`Num 33.2;;

type num_ou_null = [ `Null | `Num of float ];;

let phi : num_ou_null = `Num ((1.0 +. sqrt 5.0) /. 2.0);;

valor_string phi;;
- : bytes = "1.61803398875"

let valor_indice_tipo v = 
  match v with
  | `Null -> 0
  | `Num _ -> 1
  | `String _ -> 2
  | _ -> 100;;

valor_indice_tipo @@ `Char 'a';;

(* Variantes com valor *)

`Num 33.4;;

`Num "pi";;

valor_string;;

let valor_string2 v = 
  match v with
  | `Null -> "(null)"
  | `Num s -> s
  | `String s -> s;;

let duas_strings v = (valor_string v) ^ (valor_string2 v);;

(*
duas_strings (`Num 2.2);;
 *)

duas_strings `Null;;

type valor = [ `Null | `Num of float | `String of string ]

(* Valores JSON e BSON *)
[title Valores JSON e BSON]

type json = [
| `String of string
| `Num of float 
| `Null
| `Bool of bool
| `Lista of json list
| `Obj of (string * json) list ]

type bson = [
| `String of string
| `Null
| `Bool of bool
| `Lista of bson list
| `Obj of (string * bson) list
| `Data of int ]

module JSON = struct
  type t = [ 
  | `String of string
  | `Num of float 
  | `Null
  | `Bool of bool
  | `Lista of t list
  | `Obj of (string * t) list ]
end

module BSON = struct
  type t = [
  | `String of string
  | `Null
  | `Bool of bool
  | `Lista of bson list
  | `Obj of (string * bson) list
  | `Data of int ]
end

(*
# JSON.`Null;;
 *)

(** 11.3 Variantes extensíveis *)

type valor = ..

type valor += String of string

type valor += Int of int | Float of float

let valor_string v = 
  match v with
  | String s -> s
  | Int i -> Printf.sprintf "%d" i
  | Float f -> Printf.sprintf "%f" f
  | _ -> "???"

(* Exemplo de uso dos tipos extensíveis *)

type msg = { 
  id : int; 
  cont : bytes 
}

type 'a msg = { 
  id : int;
  cont: 'a 
}

type conteudo = String of string | Arq of string * string

type msg = ..

type msg += 
| Parar
| AbrirArq of string * string
| Dormir of int

let processar_msg m = 
  match m with
  | Parar -> parar_aplicacao ()
  | AbrirArq (dir, nome) -> abrir_arquivo dir nome
  | Dormir segs -> dormir segs
  | _ -> failwith "Tipo de mensagem inesperado"

(** 11.4 Variantes de tipos variantes *)

(* sem exemplos de código nesta seção *)
