(* 
 * titanic.ml
 * 
 *)

let arq_treino = "train.csv"
let arq_teste  = "test.csv"

type classe = Primeira | Segunda | Terceira 
type genero = Masc | Fem
type porto = Cherbourg | Queenstown | Southampton | NaoEspec

type passageiro = { 
  id         : int;
  sobreviveu : bool;
  classe     : classe;
  nome       : string;
  gen        : genero;
  idade      : float;
  irmpar     : int;
  paisfilhos : int;
  passagem   : string;
  preco      : float;
  cabine     : string;
  embarque   : porto
}

(* Funcoes para explorar os dados *)
let dados_em_falta csv = 
  let linha i lin = 
    List.concat @@ List.mapi (fun j el -> if el = "" then [(i, j)] else []) lin
  in
  List.concat @@ List.mapi linha csv 

let colunas_em_falta csv = 
  let faltas = dados_em_falta csv in
  let module IntSet = Set.Make (struct type t = int let compare = compare end) in
  let set_lista s = IntSet.fold (fun el lis -> el :: lis) s [] in
  List.fold_left (fun set (lin, col) -> IntSet.add col set) IntSet.empty faltas
  |> set_lista 

(* funcoes de leitura *)
let ler_sobr s = if s = "0" then false else true
let ler_classe s = 
  match s with
  | "1" -> Primeira
  | "2" -> Segunda
  | "3" -> Terceira
  | _ -> failwith "Classe inexistente"

let ler_genero s = if s = "male" then Masc else Fem 

let ler_idade s = 
  try
    float_of_string s
  with 
    Failure _ -> 0.0

let ler_embarq s = 
  match s with 
  | "C" -> Cherbourg
  | "Q" -> Queenstown
  | "S" -> Southampton
  | _ -> NaoEspec

let ler_dados nome = 
  let csv = Csv.load nome in
  let ler_pass [id; sobr; cls; nome; gen; idade; ip; pf; pass; prec; cab; emb] = 
    { id = int_of_string id; sobreviveu = ler_sobr sobr; classe = ler_classe cls; 
      nome = nome; gen = ler_genero gen; idade = ler_idade idade; 
      irmpar = int_of_string ip; paisfilhos = int_of_string pf; passagem = pass;
      preco = float_of_string prec; cabine = cab; embarque = ler_embarq emb } in
  List.map ler_pass @@ List.tl csv 

