(* 
 *
 * arvgenero.ml
 * Classificador baseado em genero usando uma arvore de decisao
 *
 * Andrei de A. Formiga, 2014-10-02
 *
 *)

let () = 
  Printf.printf "Lendo arquivo de teste e escrevendo genero.csv\n";
  Titanic.classifica_arv_genero "data/test.csv" 
  
