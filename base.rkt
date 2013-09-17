#lang racket

;; Este arquivo contém o código inicial com as definições de tipos, constantes
;; e algumas funções.
;;
;; Para implementar pontos e níveis é necessário alterar este arquivo
;; modificando a definição do tipo tetris.
;;
;; Não é necessário alterar este arquivo se estas características não forem
;; implementadas.

(require "tetra-tipos.rkt")
(require 2htdp/image)

(provide (struct-out posn)
         (struct-out tetramino)
         (struct-out tetris)
         make-linha
         make-campo
         make-tetris
         centraliza
         LARGURA-PADRAO ALTURA-PADRAO
         TIMEOUT-PADRAO
         Q-ALTURA Q-LARGURA
         TIMEOUT-PADRAO
         BLANK
         CORES
         I_COR J_COR L_COR O_COR S_COR T_COR Z_COR
         I J L O S T Z
         TETRAMINOS)

;; Cor - representa a cor de um quadrado em um campo de jogo
;; - Natural 0..7
;; - o valor 0 representa que o quadrado está vazio
;; - os valores 1..7 representam que o quadrado está preenchido

;; Linha - representa uma linha no campo de jogo
;; Uma linha é
;; - empty; ou
;; - (cons cor linha)
;; Ou seja, uma linha é uma lista de cores.

;; Campo - representa um campo no jogo tetris
;; Um campo é
;; - empty; ou
;; - (cons linha campo)
;; Ou seja, um campo é uma lista de linhas.

;; Observe que nestes casos não é necessário criar um tipo explicitamente, isto
;; porque cor, linha e campo são definido usando tipos existentes.

;; Template de função para o tipo linha
#;
(define (fn-linha linha)
  (cond
    [(empty? linha) ...]
    [else
     ... 
     (first linha) ...
     (fn-linha (rest linha)) ...]))

;; Template de função para o tipo campo
#;
(define (fn-campo campo)
  (cond
    [(empty? campo) ...]
    [else
     ... 
     (fn-linha (first campo)) ...
     (fn-campo (rest campo)) ...]))

;; Posn - representa a posição de um quadrado no campo
;; - lin é o número da linha começando com 0
;; - col é o número da coluna començando com 0
(struct posn (lin col) #:transparent)

;; Template de função para o tipo posn
#;
(define (fn-posn p)
  (... (p-lin p) ...
       (p-col p)))

;; Tetramino - representa uma tetraminó no jogo tetris
;; - tipo é uma lista com todas as representações (rotações) de um tetraminó.
;;   Cada representação consiste de uma lista de listas com o valor 1 nas
;;   posições ocupadas pelo tetraminó. Veja as constantes definidas no arquivo
;;   tetra-tipos.rkt e as constantes I J L O S T Z neste arquivo
;; - rot (Natural) é a rotação, que é utilizado como indíce da lista tipo para
;;   selecionar a representação desta rotação
;; - pos (Posn) é o deslocamento em relação a origem do tetraminó no campo
;;
;; Esta representação foi criada para facilitar a implementação das funções.
;; Veja a descrição da função tetramino->lista-pos (no arquivo tetris.rkt) para
;; entender como esta representação funciona.
(struct tetramino (tipo rot pos cor) #:transparent)

;; Template de função para o tipo tetramino
#;
(define (fn-tetramino t)
  (... (tetramino-tipo t) ...
       (tetramino-rot t) ...
       (tetramino-pos t)
       (tetramino-cor t)))

;; Tetris - representa um jogo tetris
;; - campo é o campo do jogo (veja a definição do tipo campo)
;; - largura e altura são as dimensões do campo
;; - tetra é o tetraminó que está caindo
;; - proximos é um stream com os próximos tetraminós
;; - timeout é um contador regressivo de ticks que controla o automovimento
(struct tetris (campo largura altura tetra proximos timeout) #:transparent)

;; Template de função para o tipo tetris
#;
(define (fn-tetris jogo)
  (... (fn-campo (tetris-campo jogo)) ...
       (tetris-largura jogo) ...
       (tetris-altura jogo) ...
       (fn-tetramino (tetris-tetra jogo)) ...
       (tetris-proximos jogo)
       (tetris-timeout jogo)...))

;; Veja alguns exemplos de criação de objetos destes tipos no arquivo
;; testes.rkt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constantes

;; Largura de um jogo tetris padrão
(define LARGURA-PADRAO 10)

;; Altura de um jogo tetris padrão
(define ALTURA-PADRAO 20)

;; Quantidade de ticks para o automovimento
(define TIMEOUT-PADRAO 20)

;; Largura de um quadrado em pixels
(define Q-LARGURA 20)

;; Altura de um quadrado em pixels
(define Q-ALTURA 20)

;; Uma imagem vazia, para ser usada como elemento neutro nas operações com
;; imagens
(define BLANK (rectangle 0 0 "solid" "black"))

;; Lista de cores usadas para desenhar os tetraminós. Cada tetraminó tem uma
;; cor (número natural) que é utilizado como indíce nesta lista. O indíce 0
;; representa que o quadrado está vazio.
(define CORES (list "black"
                    "cyan"
                    "blue"
                    "orange"
                    "yellow"
                    "green"
                    "purple"
                    "red"))

;; Tipo Cor -> Tetramino
;; Cria um tetramino na posição 0 0, rotação 0 e com a cor e o tipo
;; especificados.
(define (make-tetramino tipo cor)
  (tetramino tipo 0 (posn 0 0) cor))

;; Cores dos tetraminós
(define I_COR 1)
(define J_COR 2)
(define L_COR 3)
(define O_COR 4)
(define S_COR 5)
(define T_COR 6)
(define Z_COR 7)

;; Tetraminós
;; Veja a página http://en.wikipedia.org/wiki/Tetris e o arquivo
;; tetra-tipos.rkt para ver a forma de cada tetraminó.
(define I (make-tetramino I_TIPOS I_COR))
(define J (make-tetramino J_TIPOS J_COR))
(define L (make-tetramino L_TIPOS L_COR))
(define O (make-tetramino O_TIPOS O_COR))
(define S (make-tetramino S_TIPOS S_COR))
(define T (make-tetramino T_TIPOS T_COR))
(define Z (make-tetramino Z_TIPOS Z_COR))

;; Lista com todos os tetraminós
(define TETRAMINOS (list I J L O S T Z))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Funções para criação de jogo

;; Natural -> Linha
;; Cria uma linha com n quadrados vazios.
(define (make-linha n)
  (make-list n 0))

;; Natural Natural -> Campo
;; Cria um campo de tamanho largura x altura com todos os quadrados vazios.
(define (make-campo largura altura)
  (make-list altura (make-linha largura)))

;; Tetramino Natural -> Tetramino
;; Centraliza tetra em uma linha com largura n.
(define (centraliza tetra n)
  (define pos (tetramino-pos tetra))
  (define nova-pos (struct-copy posn pos (col (quotient (- n 3) 2))))
  (struct-copy tetramino tetra (pos nova-pos)))

;; Natural Natural Stream(Tetramino) -> Tetris
;; Cria um jogo tetris de tamanho largura x altura com tetra sendo o primeiro
;; tetramino de tetras, e proximos o restante de tetras. O tetra é centralizado
;; usando a função tetramino-centraliza.
(define (make-tetris largura altura tetras timeout)
  (tetris (make-campo largura altura)
          largura
          altura
          (centraliza (stream-first tetras) largura)
          (stream-rest tetras)
          timeout))
