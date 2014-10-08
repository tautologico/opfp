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

(*** Funcoes para detectar dados faltantes ***)

let dados_em_falta csv = 
  let linha i lin = 
    List.concat @@ List.mapi (fun j el -> if el = "" then [(i, j)] else []) lin
  in
  List.concat @@ List.mapi linha csv 

let dados_em_falta csv = 
  let coluna i j el = if el = "" then [(i, j)] else [] in 
  let linha i lin = 
    lin |> List.mapi (coluna i) |> List.concat
  in
  csv |> List.mapi linha |> List.concat 

let colunas_em_falta csv = 
  let faltas = dados_em_falta csv in
  let module IntSet = 
    Set.Make (struct type t = int let compare = compare end) 
  in
  let set_lista s = IntSet.fold (fun el lis -> el :: lis) s [] in
  let adiciona_col set (lin, col) = IntSet.add col set in
  faltas
  |> List.fold_left adiciona_col IntSet.empty 
  |> set_lista 


(*** funcoes de leitura ***)

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
  let ler_pass [id; sobr; cls; nome; gen; idade; ip; pf; pass; 
                prec; cab; emb] = 
    { id = ler_int id; 
      sobreviveu = ler_sobr sobr; 
      classe = ler_classe cls; 
      nome = nome; 
      gen = ler_genero gen; 
      idade = ler_float idade; 
      irmpar = ler_int ip; 
      paisfilhos = ler_int pf; 
      passagem = pass;
      preco = ler_float prec; 
      cabine = cab; 
      embarque = ler_embarq emb } 
  in
  List.map ler_pass @@ List.tl csv 

(* Dados de teste nao contem a coluna de sobrevivencia *)
let ler_dados_teste nome = 
  let csv = Csv.load nome in
  let ler_pass [id; cls; nom; gen; idad; ip; pf; pass; prec; cab; emb] = 
    { id = ler_int id; sobreviveu = false; classe = ler_classe cls; 
      nome = nom; gen = ler_genero gen; idade = ler_float idad; 
      irmpar = ler_int ip; paisfilhos = ler_int pf; passagem = pass;
      preco = ler_float prec; cabine = cab; embarque = ler_embarq emb } 
  in
  List.map ler_pass @@ List.tl csv 


(*** Exploração dos dados ***)

let homem p = p.gen <> Some Fem
let mulher p = p.gen = Some Fem

let apenas_homens d = 
  List.filter homem d 

let apenas_mulheres d = 
  List.filter mulher d 

(* calcula a taxa de sobrevivencia de passageiros que satisfazem um predicado *)
let taxa_sobrev_pred pred d = 
  let pass = List.filter pred d in 
  let sobr = 
    pass |> List.fold_left (fun s p -> 
                            if p.sobreviveu then s + 1 
                            else s) 0 
  in
  (float sobr) /. (float @@ List.length pass)

(* calcula a taxa de sobrevivencia geral *)
let taxa_sobrevivencia d = 
  taxa_sobrev_pred (fun p -> true) d

(* Um classificador simples *)
let sobrevivencia_por_genero d = 
  d |> List.map (fun p -> 
                 if mulher p then (p.id, 1) 
                 else (p.id, 0))

let escreve_resultado res nome = 
  let arqout = open_out nome in
  Printf.fprintf arqout "PassengerId,Survived\n"; 
  List.iter (fun (id, s) -> Printf.fprintf arqout "%d,%d\n" id s) res;
  close_out arqout

let classifica_teste_por_genero () = 
  let dados_teste = ler_dados_teste arq_teste in
  let resultado = sobrevivencia_por_genero dados_teste in
  escreve_resultado resultado "genero.csv" 

(* Separacao por faixas de preco *)
type faixa = F0a9 | F10a19 | F20a29 | F30maior 

let calcula_faixa prec = 
  if prec >= 30.0 then F30maior
  else if prec >= 20.0 then F20a29
  else if prec >= 10.0 then F10a19 
  else F0a9

let taxa_sobrev_faixa d f = 
  taxa_sobrev_pred (fun p -> calcula_faixa p.preco = f) d

let sobrev_faixas d = 
  List.map (fun f -> taxa_sobrev_faixa d f) [F0a9; F10a19; F20a29; F30maior]


(* Especificando arvores manualmente *)

(* arvore de decisao com resultado booleano *)
type teste = passageiro -> arvdec
and arvdec = Teste of teste | Result of bool 

(* arvore de classificacao por genero *)
let arv_genero = 
  Teste (fun p -> if p.gen = Some Fem then Result true
                  else Result false)

(* classificador usando arvores de decisao *) 
let rec aplica_arvore arv p = 
  match arv with
  | Result true -> (p.id, 1)
  | Result false -> (p.id, 0)
  | Teste t -> aplica_arvore (t p) p

let aplica_arvore_dados arv d = 
  List.map (aplica_arvore arv) d 

let classifica_arv_genero () = 
  let dados_teste = ler_dados_teste arq_teste in
  let resultado = aplica_arvore_dados arv_genero dados_teste in
  escreve_resultado resultado "genero.csv"


(* Algoritmo ID3 *)

(* logaritmo de base 2 *)
let log2 n = (log10 n) /. (log10 2.0)

(* Conta quantos sobreviveram e nao sobreviveram no conjunto de passageiros s *)
let contagem_classes s = 
  let conta (t, f) p = 
    if p.sobreviveu then (t+1, f) else (t, f+1)
  in 
  List.fold_left conta (0, 0) s

(* Calcula a entropia H(s) de um conjunto de passageiros s *)
let entropia s = 
  let t, f = contagem_classes s in
  let tf, ff = float t, float f in
  let tfrac = tf /. (tf +. ff) in
  let ffrac = ff /. (tf +. ff) in
  if p = 0 || n = 0 then 0.0
  else -. (tfrac *. log2 tfrac) -. (ffrac *. log2 ffrac)
  
(* Calcula a entropia apos dividir o conjunto por um teste *)
