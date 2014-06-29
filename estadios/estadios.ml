(*

Informacoes sobre utilizacao e custos dos estadios brasileiros na Copa 2014

Dados em
http://fivethirtyeight.com/features/were-the-billions-brazil-spent-on-world-cup-stadiums-worth-it/

*)

type estadio = {
  nome :      string;
  capacidade: float;
  custo:      float;
  pagantes:   float;
  sui:        float;
  fci:        float
}

let nome e = e.nome
let capacidade e = e.capacidade 
let custo e = e.custo
let pagantes e = e.pagantes
let sui e = e.sui
let fci e = e.fci

let tupla_estadio (nome, capacidade, custo, pagantes, sui, fci) = 
  { nome; capacidade; custo; pagantes; sui; fci }

let br_estadios = List.map tupla_estadio [
  (* nome            Cap.    Custo  Pagantes  SUI    FCI     *)
  ("Maracana"      , 71.2e3, 467e6, 791e3,    11.1,     590.0);
  ("Mane Garrincha", 65.7e3, 622e6, 14e3,      0.2,   44565.0);
  ("Corinthians",    60.0e3, 459e6, 559e3,     9.3,     821.0);
  ("Castelao",       57.7e3, 231e6, 275e3,     4.8,     839.0);
  ("Mineirao",       56.1e3, 295e6, 197e3,     3.5,    1496.0);
  ("Fonte Nova",     49.3e3, 263e6, 432e3,     8.8,     609.0);
  ("Beira-Rio",      42.2e3, 147e6, 346e3,     8.2,     425.0);
  ("Pernambuco",     40.6e3, 236e6, 221e3,     5.4,    1067.0);
  ("Amazonia",       39.6e3, 298e6,   2e3,     0.1,  150050.0);
  ("Pantanal",       39.6e3, 253e6,   3e3,     0.1,   94579.0);
  ("Dunas",          39.3e3, 178e6, 208e3,     5.3,     856.0);
  ("Baixada",        37.6e3, 160e6, 269e3,     7.2,     594.0)
]

let media l = 
  let soma = List.fold_left (+.) 0.0 l in
  soma /. (float @@ List.length l)

let media_sui estadios = 
  let media_cap = media @@ List.map (fun e -> e.capacidade) estadios in
  let media_pag = media @@ List.map pagantes estadios in
  media_pag /. media_cap

let media_fci estadios = 
  let media_custo = media @@ List.map (fun e -> e.custo) estadios in 
  let media_pag = media @@ List.map pagantes estadios in
  media_custo /. media_pag 

(* Retirando os estadios com utilizacao minima *)
let estadios_utilizados = List.filter (fun e -> e.sui >= 1.0) br_estadios

