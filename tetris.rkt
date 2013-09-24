#lang racket

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
(require "tetra-tipos.rkt")
(require htdp/matrix)

(provide make-tetris-padrao
         tetramino->lista-pos
         lop-validas?
         lop-livres?
         fixa
         limpa
         trata-tecla
         trata-tick
         desenha
         linha-um
         campo-um
         incrementa
         linha-completa?
         campo-completas
         move-direita
         move-esquerda
         move-baixo
         desce-tudo
         rotaciona
         movimenta?
         preenche
         proximo
         stream-tetraminos
         desenha-tetra
         varre-linha
         varre-campo
         quadrado
         pinta-quadrado
         game-over?)


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
  (cond
    [(key=? tecla "right") (move-direita jogo)]
    [(key=? tecla "left") (move-esquerda jogo)]
    [(key=? tecla "up") (rotaciona jogo)]
    [(key=? tecla "down") (move-baixo jogo)]
    [(key=? tecla " ") (desce-tudo jogo)]
    [else
     jogo]))



;; jogo -> jogo
;; Verifica se a movimentação pode ser realizada
;; (define (movimenta? jogo) )
(define (movimenta? tetra jogo)
  (define tetra-jogo (tetris-tetra jogo))
  (define largura (tetris-largura jogo))
  (define altura (tetris-altura jogo))
  (define pos-ocupada (tetramino->lista-pos tetra))
  (define campo-jogo (tetris-campo jogo))
  (if (and (lop-validas? pos-ocupada largura altura) (lop-livres? pos-ocupada campo-jogo))
      true
      false))

  

;; jogo -> jogo
;; Move o tetraminó para a direita, se der
;; (define (move-direita jogo) jogo)
(define (move-direita jogo)
  (define tetramino-jogo (tetris-tetra jogo))
  (define pos-atual (tetramino-pos tetramino-jogo))
  (define pos_nova (struct-copy posn pos-atual
                                [col (add1 (posn-col pos-atual))]))
  (define tetra-novo (struct-copy tetramino tetramino-jogo
                                  [pos pos_nova]))
  (cond
    [(not (movimenta? tetra-novo jogo)) jogo]
    [else
     (struct-copy tetris jogo
                     [tetra tetra-novo])]))


;; jogo -> jogo
;; Move o tetraminó para a esquerda
;; (define (move-esquerda jogo) jogo)
(define (move-esquerda jogo)
  (define tetramino-jogo (tetris-tetra jogo))
  (define pos-atual (tetramino-pos tetramino-jogo))
  (define pos_nova (struct-copy posn pos-atual
                                [col (sub1 (posn-col pos-atual))]))
  (define tetra-novo (struct-copy tetramino tetramino-jogo
                                  [pos pos_nova]))
  (cond
    [(not (movimenta? tetra-novo jogo)) jogo]
    [else
     (struct-copy tetris jogo
                     [tetra tetra-novo])]))


;; jogo -> jogo
;; Rotaciona o tetraminó
;; (define (rotaciona jogo) jogo)
(define (rotaciona jogo)
  (define tetramino-jogo (tetris-tetra jogo))
  (define rot-atual (tetramino-rot tetramino-jogo))
  (define tipo (tetramino-tipo tetramino-jogo))
  (if (equal? tipo I_TIPOS)
      (if (equal? rot-atual 1)
          (set! rot-atual (sub1 rot-atual))
          (set! rot-atual (add1 rot-atual)))
      (if (equal? tipo O_TIPOS)
          rot-atual
          (if (equal? rot-atual 3)
              (set! rot-atual (- rot-atual 3))
              (set! rot-atual (add1 rot-atual)))))
  (define tetra-novo (struct-copy tetramino tetramino-jogo
                                  [rot rot-atual]))
  (cond
    [(not (movimenta? tetra-novo jogo)) jogo]
    [else
     (struct-copy tetris jogo
                     [tetra tetra-novo])]))
          
     
;; jogo -> jogo
;; Move o tetraminó uma posição para baixo
;; (define (move-baixo jogo) jogo)
(define (move-baixo jogo)
  (cond
    [(empty? jogo) empty]
    [else
     (define prox (proximo (limpa (fixa jogo))))
     (define tetramino-jogo (tetris-tetra jogo))
     (define pos-atual (tetramino-pos tetramino-jogo))
     (define pos_nova (struct-copy posn pos-atual
                                   [lin (add1 (posn-lin pos-atual))]))
     (define tetra-novo (struct-copy tetramino tetramino-jogo
                                  [pos pos_nova]))
     (cond
       [(movimenta? tetra-novo jogo) (struct-copy tetris jogo
                                                  [tetra tetra-novo]
                                                  [timeout TIMEOUT-PADRAO])]
       [else
        (struct-copy tetris prox
                     [timeout TIMEOUT-PADRAO])])]))


;; jogo -> jogo
;; Move o tetraminó de uma vez para baixo
;; (define (desce-tudo jogo) jogo)
(define (desce-tudo jogo)
  (cond
    [(empty? jogo) empty]
    [else
     (define novo-jogo (move-baixo jogo))
     (define tetra (tetris-tetra jogo))
     (cond
       [(not (equal? (tetris-campo jogo) (tetris-campo novo-jogo))) jogo]
       [else
        (desce-tudo novo-jogo)])]))


(define (game-over? jogo)
  (define campo-jogo (tetris-campo jogo))
  (define proximo (centraliza (stream-first (tetris-proximos jogo)) LARGURA-PADRAO))
  (define lista-posicoes-proximo (tetramino->lista-pos proximo))
  (if (lop-livres? lista-posicoes-proximo campo-jogo)
      false
      true))
  


;; Jogo -> Jogo
;; Função que trata um tick. Esta função é chamada 28 vezes por segundo, ela
;; deve mover o tetra para baixo depois que uma determinada quantidade
;; (TIMEOUT-PADRAO) de ticks. Se o jogador mover para baixo e fixar o
;; tetraminó, a contagem deve reiniciar.
(define (trata-tick jogo)
  (define timeout-jogo (tetris-timeout jogo))
  (cond
    [(zero? timeout-jogo) (move-baixo jogo)]
    [else
     (struct-copy tetris jogo
                  [timeout (sub1 timeout-jogo)])]))
     
  
  

;; Tetris -> Imagem
;; Esta função é chamada quando o jogo precisa ser desenhado na tela. Devolve
;; uma imagem que representa o jogo.
;; Veja as funções pré-definidas rectangle, beside, above e overlay no pacote
;; 2htdp/image.
(define (desenha jogo)
  (define campo (tetris-campo jogo))
  (define tetra-jogo (tetris-tetra jogo))
  (define lista-jogo (tetramino->lista-pos tetra-jogo))
  (overlay/align "left" "top" (desenha-tetra tetra-jogo lista-jogo)
                  (varre-campo campo))
  )



(define (desenha-tetra tetra lista)
  (cond
    [(empty? lista) BLANK]
    [else
     (define cor (tetramino-cor tetra))
     (overlay/xy (quadrado cor) (* -40 (posn-col (first lista))) (*  (posn-lin (first lista)) -40) (desenha-tetra tetra (rest lista)))]))

(define (varre-linha linha)
  (cond
    [(empty? linha) BLANK]
    [else
     (beside (quadrado (list-ref linha 0))
             (varre-linha (rest linha)))]))

(define (varre-campo campo)
  (cond
    [(empty? campo) BLANK]
    [else
     (above (varre-linha (list-ref campo 0))
            (varre-campo (rest campo)))]))


;; Cor -> Quadrado
;; Desenha o quadrado referente a posição
;; (define (quadrado cor))
(define (quadrado cor)
  (cond
    [(= cor 1) (pinta-quadrado "cyan" "teal")]
    [(= cor 2) (pinta-quadrado "blue" "RoyalBlue")]
    [(= cor 3) (pinta-quadrado "orange" "chocolate")]
    [(= cor 4) (pinta-quadrado "yellow" "goldenrod")]
    [(= cor 5) (pinta-quadrado "green" "aquamarine")]
    [(= cor 6) (pinta-quadrado "purple" "mediumpurple")]
    [(= cor 7) (pinta-quadrado "red" "tomato")]
    [else
     (square 40 "solid" "black")]))


;; Cor1 Cor2 -> quadrado
;; Define as cores e as formas do quadrado
;; (define (pinta-quadrado cor1 cor2) quadrado)
(define (pinta-quadrado cor1 cor2)
  (overlay (overlay (square 25 "outline" "white")
                    (square 25 "solid" cor1))
           (add-line (add-line (overlay (square 40 "outline" "white")
                                        (square 40 "solid" cor2))
                               0 40 40 0 "white")
                     40 40 0 0 "white")))

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
;; '((0 1 0)    ; 0              
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
  (incrementa (campo-um (list-ref (tetramino-tipo t) (tetramino-rot t)) 0) (tetramino-pos t)))

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
;; (define (linha-um linha lin col) )
(define (linha-um linha lin col)
  (cond
    [(empty? linha) empty]
    [(zero? (first linha)) (linha-um (rest linha) lin (add1 col))]
    [else
     (cons (posn lin col) (linha-um (rest linha) lin (add1 col)))]))

;; Campo -> Lista de posições
;; Retorna uma lista com as posições em que as linhas do campo tem o valor 1
;; (define (campo-um campo lin) )
(define (campo-um campo lin)
  (cond
    [(empty? campo) empty]
    [else
     (flatten (cons (linha-um (first campo) lin 0) (campo-um (rest campo) (add1 lin))))]))



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
;; Fixa as posições ocupadas pelo tetraminó (que está caindo) no campo do
;; jogo, ou seja, sobreescreve o jogo com o novo tetraminó.
;; Requer que tetraminó não possa ser movido para baixo.
(define (fixa jogo)
  (cond
    [(game-over? jogo) (make-tetris-padrao)]
    [else
     (define tetramin (tetris-tetra jogo))
     (define cor (tetramino-cor tetramin))
     (define campo (tetris-campo jogo))
     (define lista-pos (tetramino->lista-pos tetramin))
     (if (empty? jogo)
         empty
         (struct-copy tetris jogo
                      [campo (preenche lista-pos campo cor)]))]))
      

;; Lista(posn) campo cor -> campo
;; Preenche as posições ocupadas pelo tetraminó (que está caindo) no campo do
;; jogo, para que a função fixa sobreescreva o jogo.
(define (preenche lista-pos campo cor)
  (if (empty? lista-pos)
      campo
      (for/list ([lin campo] [i [in-naturals]])  
           [for/list ([elemento lin] [j [in-naturals]]) 
             (if (list? (member (posn i j) lista-pos)) cor elemento)])))

;; Jogo -> Jogo
;; Devolve um jogo sem as linhas que estão completas, isto é, as linhas que não
;; tem nenhum quadrado vazio. O jogo devolvido tem o mesmo tamanho do jogo de
;; entrada.
(define (limpa jogo)
  (define novo-campo (campo-completas (tetris-campo jogo)))
  (cond
    [(equal? (length novo-campo) (length (tetris-campo jogo))) jogo]
    [else
     (struct-copy tetris jogo (campo (adiciona-linha novo-campo 
                                                     (- (length (tetris-campo jogo)) (length novo-campo)) 
                                                     (tetris-largura jogo))))]))

;; campo -> campo
;; Adiciona a linha no campo
;; (define (adiciona-linha campo l) )
(define (adiciona-linha campo numLinhas largura)
  (cond
    [(zero? numLinhas) campo]
    [else
     (cons (make-list largura 0) (adiciona-linha campo (sub1 numLinhas) largura))]))


;; Linha -> Boolean
;; Verifica se a linha está completa ou não
;; (define (linha-completa linha) )
(define (linha-completa? linha)
  (cond
    [(empty? linha) true]
    [else
     (if (= (first linha) 0)
         false
         (linha-completa? (rest linha)))]))

;; Campo -> Lista
;; Retorna as listas que estão completas no campo
;; (define (campo-completas campo) )
(define (campo-completas campo)
  (cond
    [(empty? campo) empty]
    [else
     (if (equal? (linha-completa? (first campo)) #t)
         (campo-completas (rest campo))
         (cons (first campo) (campo-completas (rest campo))))]))

;; -> Stream(Tetramino)
;; Cria um stream randômico de tetraminós.
;; Esta função não precisa de testes.
;; Você tem que implementar esta função, o corpo incial deve ser descartado.
(define (stream-tetraminos)
  (define num-aleatorio (random 7))
  (define tetra-aleatorio (list-ref TETRAMINOS num-aleatorio))
  (stream-cons tetra-aleatorio (stream-tetraminos)))

;; jogo -> jogo
;; Função que faz com que o próximo tetraminó entre, apos o anterior ser afixado.
;; (define (proximo jogo) jogo)
(define (proximo jogo)
  (define lista-prox (tetris-proximos jogo))
  (struct-copy tetris jogo
               [tetra (centraliza (stream-first lista-prox) LARGURA-PADRAO)]
               [proximos (stream-rest lista-prox)]))
