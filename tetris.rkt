#lang racket
(require htdp/matrix)

;; Você deve implementar as funções neste arquivo. Novas funções podem ser
;; criadas, mas todas as funções devem ter testes (no arquivo testes.rkt).
;;
;; Observe que algumas destas funções não tem testes, faz parte do trabalho
;; criar estes testes.
;;
;; Você não precisa se preocupar com ler a tecla pressionada ou desenhar o jogo
;; na tela. O arquivo main.rkt chama uma função que faz isso. Basta você
;; implementar as funções deste arquivo que o jogo funciona.
;;
;; Para ter uma ideia do processo de execução do jogo, execute o arquivo
;; main.rkt sem mudar nada neste arquivo. Uma janela irá aparecer. Pressione
;; algumas teclas e observe a saída no console do DrRacket. Veja o corpo
;; inicial das funções make-tetris-padrao, trata-tecla, trata-tick e desenha.

(require "base.rkt")
(require 2htdp/image)
(require 2htdp/universe)

(provide make-tetris-padrao
         tetramino->lista-pos
         lop-validas?
         lop-livres?
         fixa
         limpa
         trata-tecla
         trata-tick
         desenha
         fn-linha
         fn-campo
         incrementa)

;; -> Tetris
;; Cria o jogo inicial.
;; Esta função é chamada no arquivo main.rkt.
(define (make-tetris-padrao)
  (make-tetris LARGURA-PADRAO ALTURA-PADRAO (stream-tetraminos) TIMEOUT-PADRAO))

;; Jogo String -> Jogo
;; Esta função é chamada quando uma tecla é pressionada.
;; Devolve um jogo com o tetraminó que está caindo movido de acordo com a tecla
;;   "right" - tenta mover para direita
;;   "left"  - tenta mover para esquerda
;;   "up"    - tenta rotacionar
;;   "down"  - tenta mover para baixo
;;
;; Se a tecla for "right", "left" ou "up" e o movimento não puder ser
;; realizado, o jogo é devolvido sem modificações.
;;
;; Se a tecla for "down" e o movimento não puder ser realizado, tetra é fixado
;; no campo, as linhas completas são removidas, o próximo tetraminó é
;; selecionada para cair e o contador de automovimento retorna ao valor
;; inicial.
;;
;; Se o movimento puder ser realizado, o jogo após o movimento é devolvido.
;;
;; Use a função key=? para comparar o tecla com os valores "right", "left, "up"
;; e "down".
(define (trata-tecla jogo tecla)
  (printf "\ntrata-tecla:~a\n" tecla)
  jogo)

;; Jogo -> Jogo
;; Função que trata um tick. Esta função é chamada 28 vezes por segundo, ela
;; deve mover o tetra para baixo depois que uma determinada quantidade
;; (TIMEOUT-PADRAO) de ticks. Se o jogador mover para baixo e fixar o
;; tetraminó, a contagem deve reiniciar.
(define (trata-tick jogo)
  (printf "t")
  jogo)

;; Tetris -> Imagem
;; Esta função é chamada quando o jogo precisa ser desenhado na tela. Devolve
;; uma imagem que representa o jogo.
;; Veja as funções pré-definidas rectangle, beside, above e overlay no pacote
;; 2htdp/image.
(define (desenha jogo)
  (printf "d")
  (rectangle (* (tetris-largura jogo) Q-LARGURA)
             (* (tetris-altura jogo) Q-ALTURA)
             "solid"
             "black"))

;; Tetramino -> Lista(Posn)
;; Devolve a lista de posições que t ocupa no campo considerando a rotação e a
;; posição (translação em relação a origem).
;; 
;; Por exemplo, seja TT1 definido como
;; (define TT1 (tetramino T_TIPOS 1 (posn 1 0) T_COR))
;; este tetraminó está na rotação 1 e na posição (posn 1 0). O elemento na
;; posição 1 de T_TIPOS é T1 que é a seguinte lista de listas (definina em
;; tetra-tipos.rkt)
;;    0 1 2     ; colunas
;;              ; linhas
;; '((0 1 0)    ; 0              (list-ref (list-ref
;;   (0 1 1)    ; 1
;;   (0 1 0)))  ; 2
;;
;; As as posições ocupadas por T1 são marcadas com 1, ou seja, as posições
;; ocupadas por T1 são (posn 0 1) (posn 1 1) (posn 1 2) e (posn 2 1). Estas São
;; as posições em relação a (posn 0 0), mas o tetraminó está na posição
;; (posn 1 0), desta forma, precisamos fazer a translação das posições. Para
;; isto, somamos o ponto (posn 1 0) a cada ponto de T1, o que resulta em
;; (posn 1 1) (posn 2 0) (posn 2 2) (posn 3 1). Observe que é posível ter
;; um deslocamento em relação a origem negativa. Por exemplo, se a posição de
;; TT1 fosse (posn 0 -1), obteríamos como resposta da função a lista com
;; as posições (posn 0 0) (posn 1 0) (posn 1 1) (posn 2 0).
;;
;; Veja os testes para outros exemplos de como esta função deve funcionar.
(define (tetramino->lista-pos t)
  (incrementa (fn-campo (list-ref (tetramino-tipo t) (tetramino-rot t)) 0) (tetramino-pos t)))

;; Lista posição -> Lista
;; Incrementa a lista com a posição desejada
;; (define (incrementa lst pos) )
(define (incrementa lst pos)
  (cond
    [(empty? lst) empty]
    [else
     (cons (posn (+ (posn-lin (first lst)) (posn-lin pos)) (+ (posn-col (first lst)) (posn-col pos)))
           (incrementa (rest lst) pos))]))


;; Linha -> Lista de posições
;; Retorna uma lista com as posições em que as linhas do campo tem o valor 1
;; (define (fn-linha linha lin col) )
(define (fn-linha linha lin col)
  (cond
    [(empty? linha) empty]
    [(zero? (first linha)) (fn-linha (rest linha) lin (add1 col))]
    [else
     (cons (posn lin col) (fn-linha (rest linha) lin (add1 col)))]))

;; Campo -> Lista de posições
;; Retorna uma lista com as posições em que as linhas do campo tem o valor 1
;; (define (fn-linha linha lin col) )
(define (fn-campo campo lin)
  (cond
    [(empty? campo) empty]
    [else
     (flatten (cons (fn-linha (first campo) lin 0) (fn-campo (rest campo) (add1 lin))))]))



;; Lista(Posn) Natural Natural -> Boolean
;; Devolve verdadeiro se todas os posições de lp são válidas, isto é, estão
;; dentro de um campo de tamanho largura x altura. Devolve falso caso
;; contrário.
(define (lop-validas? lp largura altura) 
  (cond
    [(empty? lp) true]
    [else
     (if (or (>= (posn-lin (first lp)) altura) (< (posn-lin (first lp)) 0))
         false
         (if (or (>= (posn-col (first lp)) largura) (< (posn-col (first lp)) 0))
             false
             (lop-validas? (rest lp) largura altura)))]))

;; Lista(Posn) Campo -> Boolean
;; Devolve verdadeiro se todas as posições de lp estão livres no campo. Devolve
;; falso caso contrário.
;; Requer que todas as posições em lp sejam válidas.
(define (lop-livres? lp campo) 
  (cond
    [(not (lop-validas? lp (matrix-cols campo) (matrix-rows campo))) false]
    [else
     (if (empty? lp)
         true
         (if (not (zero? (list-ref (list-ref campo (posn-lin (first lp))) (posn-col (first lp)))))
             false
             (lop-livres? (rest lp) campo)))]))
     

;; Jogo -> Jogo
;; Preenche as posições ocupadas pelo tetraminó (que está caindo) no campo do
;; jogo.
;; Requer que tetraminó não possa ser movido para baixo.
(define (fixa jogo) jogo)

;; Jogo -> Jogo
;; Devolve um jogo sem as linhas que estão completas, isto é, as linhas que não
;; tem nenhum quadrado vazio. O jogo devolvido tem o mesmo tamanho do jogo de
;; entrada.
(define (limpa jogo) jogo)

;; -> Stream(Tetramino)
;; Cria um stream randômico de tetraminós.
;; Esta função não precisa de testes.
;; Você tem que implementar esta função, o corpo incial deve ser descartado.
(define (stream-tetraminos)
  (stream-cons T empty-stream))
