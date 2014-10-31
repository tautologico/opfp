Compilação e Interpretação
====

Este diretório contém os exemplos de código do Capítulo 6 do livro, 
Interpretador e Compilador. 

## Testes

Para executar os testes é preciso ter a biblioteca ounit instalada, o que
pode ser feito facilmente usando OPAM.

Para compilar os testes é preciso configurar com essa opção, e depois
compilar o projeto normalmente:

````
ocaml setup.ml -configure --enable-tests
ocaml setup.ml -build
````

Depois disso, a bateria de testes pode ser executada executando o arquivo
test.native ou usando o setup.ml:

````
ocaml setup.ml -test
````
