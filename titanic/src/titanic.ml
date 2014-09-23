(* 
 * titanic.ml
 * 
 *)

let arq_treino = "train.csv"
let arq_teste  = "test.csv"

type classe = Primeira | Segunda | Terceira 
type genero = Masc | Fem
type porto = Cherbourg | Queenstown | Southampton

type passageiro = { 
  id         : int;
  sobreviveu : bool;
  classe     : classe;
  nome       : string;
  gen        : genero option;
  idade      : float;
  irmpar     : int;
  paisfilhos : int;
  passagem   : string;
  preco      : float;
  cabine     : string;
  embarque   : porto option
}

(* Funcoes para detectar dados faltantes *)
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

let ler_genero s = 
  match s with
  | "male" -> Some Masc
  | "female" -> Some Fem
  | _ -> None

let ler_int s = 
  try
    int_of_string s 
  with
    Failure _ -> 0

let ler_float s = 
  try
    float_of_string s
  with 
    Failure _ -> 0.0

let ler_embarq s = 
  match s with 
  | "C" -> Some Cherbourg
  | "Q" -> Some Queenstown
  | "S" -> Some Southampton
  | _ -> None 

let ler_dados_treino nome = 
  let csv = Csv.load nome in
  let ler_pass [id; sobr; cls; nome; gen; idade; ip; pf; pass; prec; cab; emb] = 
    { id = ler_int id; sobreviveu = ler_sobr sobr; classe = ler_classe cls; 
      nome = nome; gen = ler_genero gen; idade = ler_float idade; 
      irmpar = ler_int ip; paisfilhos = ler_int pf; passagem = pass;
      preco = ler_float prec; cabine = cab; embarque = ler_embarq emb } in
  List.map ler_pass @@ List.tl csv 

(* TODO: coluna sobreviveu? *)
let ler_dados_teste nome = 
  let csv = Csv.load nome in
  let ler_pass [id; cls; nome; gen; idade; ip; pf; pass; prec; cab; emb] = 
    { id = ler_int id; sobreviveu = false; classe = ler_classe cls; 
      nome = nome; gen = ler_genero gen; idade = ler_float idade; 
      irmpar = ler_int ip; paisfilhos = ler_int pf; passagem = pass;
      preco = ler_float prec; cabine = cab; embarque = ler_embarq emb } in
  List.map ler_pass @@ List.tl csv 

(* Exploração dos dados *)
let apenas_homens d = 
  List.filter (fun p -> p.gen <> Some Fem) d 

let apenas_mulheres d = 
  List.filter (fun p -> p.gen = Some Fem) d 

let taxa_sobrevivencia d = 
  let sobr = List.fold_left (fun s p -> if p.sobreviveu then s + 1 else s) 0 d in
  (float sobr) /. (float @@ List.length d)

(* Um classificador simples *)
let sobrevivencia_por_genero d = 
  List.map (fun p -> if p.gen = Some Fem then (p.id, 1) else (p.id, 0)) d 

let escreve_resultado res nome = 
  let arqout = open_out nome in
  Printf.fprintf arqout "PassengerId,Survived\n"; 
  List.iter (fun (id, s) -> Printf.fprintf arqout "%d,%d\n" id s) res;
  close_out arqout

let classifica_teste_por_genero () = 
  let dados_teste = ler_dados_teste arq_teste in
  let resultado = sobrevivencia_por_genero dados_teste in
  escreve_resultado resultado "genero.csv" 

(* Especificando arvores manualmente *)
