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

(* funcoes de leitura *)
let ler_sobr s = if s = "0" then false else true
let ler_classe s = 
  match s with
  | "1" -> Primeira
  | "2" -> Segunda
  | "3" -> Terceira
  | _ -> failwith "Classe inexistente"

let ler_genero s = if s = "male" then Masc else Fem 
let ler_embarq s = 
  match s with 
  | "C" -> Cherbourg
  | "Q" -> Queenstown
  | "S" -> Southampton
  | _ -> failwith "Porto nao reconhecido"

let ler_dados nome = 
  let csv = Csv.load nome in
  let ler_pass [id; sobr; cls; nome; gen; idade; ip; pf; pass; prec; cab; emb] = 
    { id = int_of_string id; sobreviveu = ler_sobr sobr; classe = ler_classe cls; 
      nome = nome; gen = ler_genero gen; idade = float_of_string idade; 
      irmpar = int_of_string ip; paisfilhos = int_of_string pf; passagem = pass;
      preco = float_of_string prec; cabine = cab; embarque = ler_embarq emb } in
  List.map ler_pass csv 

