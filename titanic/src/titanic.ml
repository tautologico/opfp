(* 
 * titanic.ml
 * 
 *)

(** Árvores de decisão e o problema do Titanic. *)


type classe = Primeira | Segunda | Terceira 
type genero = Masc | Fem
type porto = Cherbourg | Queenstown | Southampton

(** Um passageiro do Titanic *)
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
  res |> List.iter (fun (id, s) -> 
                    Printf.fprintf arqout "%d,%d\n" id s);
  close_out arqout

let classifica_teste_por_genero arqteste arqsaida = 
  let dados_teste = ler_dados_teste arqteste in
  let resultado = sobrevivencia_por_genero dados_teste in
  escreve_resultado resultado arqsaida

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


(* Arvores de decisao *)
(* Especificando arvores manualmente *)

(** Um teste em uma arvore de decisao *)
type teste = { 
  nome : string;           (* nome do teste *)
  f : passageiro -> int;   (* funcao de teste *)
  nvals : int              (* numero de valores do teste *)
}

type arvdec = Teste of teste * arvdec list | Result of bool 

(** Função de teste para classificacao por gênero. *)
let test_f_genero p = 
  if p.gen = Some Fem then 0 else 1

let teste_genero = {
  nome = "genero";
  f = test_f_genero;
  nvals = 2;
}

let arv_genero = 
  Teste (teste_genero, [Result true; Result false])

(** [aplica_arvore arv p] classifica o passageiro [p] usando 
    a árvore de decisão [arv]. *)
let rec aplica_arvore arv p = 
  match arv with
  | Result true -> (p.id, 1)
  | Result false -> (p.id, 0)
  | Teste (t, l) -> aplica_arvore (List.nth l (t.f p)) p

let aplica_arvore_dados arv d = 
  List.map (aplica_arvore arv) d 

let classifica_arv_genero arqteste arqsaida = 
  let dados_teste = ler_dados_teste arqteste in
  let resultado = aplica_arvore_dados arv_genero dados_teste in
  escreve_resultado resultado arqsaida


(* Algoritmo ID3 *)

(** Logaritmo de base 2. *)
let log2 n = (log10 n) /. (log10 2.0)

(** Conta quantos sobreviveram e nao sobreviveram no conjunto de 
    passageiros [s]. *)
let contagem_classes s = 
  let conta (t, f) p = 
    if p.sobreviveu then (t+1, f) else (t, f+1)
  in 
  List.fold_left conta (0, 0) s

(** Calcula a entropia H(s) de um conjunto de passageiros [s]. *)
let entropia s = 
  let t, f = contagem_classes s in
  let tf, ff = float t, float f in
  let tfrac = tf /. (tf +. ff) in
  let ffrac = ff /. (tf +. ff) in
  if t = 0 || f = 0 then 0.0
  else -. (tfrac *. log2 tfrac) -. (ffrac *. log2 ffrac)

(** Particiona o conjunto [s] em subconjuntos de acordo com o teste [t]. *)
let particao t s = 
  let atualiza_part a res p = 
    a.(res) <- p:: a.(res)
  in
  let arr = Array.make t.nvals [] in
  let results = List.map t.f s in
  List.iter2 (atualiza_part arr) results s;
  Array.to_list arr

(** Calcula a entropia apos dividir o conjunto [s] pelo teste [t]. *)
let entrop_teste s t = 
  let part = particao t s in
  let entrop_valor si ac = 
    let frac = (float @@ List.length si) /. 
               (float @@ List.length s) in
    ac +. frac *. (entropia si)
  in
  List.fold_right entrop_valor part 0.0 

(** Retorna o indice do item da lista [l] com o maior valor 
    quando aplicado a funcao [f]. *)
let max_f f l = 
  let rec loop l i mi max = 
    match l with
    | [] -> (mi, max)
    | e :: t -> 
       let fe = f e in
       if fe > max then loop t (i+1) i fe 
       else loop t (i+1) mi max
  in
  loop l 0 0 neg_infinity

(** Remove o item de indice [ix] na lista [l]. *)
let rec remove_indice ix l = 
  match ix, l with
  | 0, h :: t -> t
  | _, [] -> failwith "Indice fora dos limites"
  | _, h :: t -> h :: remove_indice (ix-1) t 
 
(** Constroi uma arvore de decisao seguindo o algoritmo ID3, 
 usando dados de treinamento [d] e a lista de testes [lt]. 
 O valor [default] e usado quando a arvore chega em um ramo vazio. *)
let id3 d lt default = 
  let seleciona_teste d lt = 
    let hd = entropia d in
    fst @@ max_f (fun t -> hd -. (entrop_teste d t)) lt 
  in
  let rec constroi_arvore lt d = 
    match contagem_classes d with
    | (0, 0) -> Result default
    | (n, 0) -> Result true
    | (0, n) -> Result false
    | (t, f) ->
       match lt with
       | [] -> if t > f then Result true else Result false
       | _ -> 
          let tix = seleciona_teste d lt in
          let teste = List.nth lt tix in
          let prox_lt = remove_indice tix lt in
          let subs = particao teste d in
          Teste (teste, List.map (constroi_arvore prox_lt) subs)
  in
  constroi_arvore lt d

(* Testes para arvores de decisao *)

let test_f_classe p = 
  match p.classe with 
  | Primeira -> 0
  | Segunda -> 1
  | Terceira -> 2

let teste_classe = {
  nome = "classe";
  f = test_f_classe;
  nvals = 3
}

let test_f_preco_4faixas p = 
  if p.preco < 10.0 then 0
  else if p.preco < 20.0 then 1
  else if p.preco < 30.0 then 2
  else 3

let teste_preco_4faixas = {
  nome = "preco em 4 faixas";
  f = test_f_preco_4faixas;
  nvals = 4
}

(** Constroi uma arvore de decisao baseado em dados de treinamento e um 
    conjunto de testes. *)
let arvore_testes arqtreino testes = 
  let d_treino = ler_dados_treino arqtreino in 
  id3 d_treino testes false

(** Classifica os dados de treino usando uma arvore de decisao. *)
let classifica_teste_arvore arv arqteste arqsaida = 
  let d_teste = ler_dados_teste arqteste in
  let resultado = aplica_arvore_dados arv d_teste in
  escreve_resultado resultado arqsaida
