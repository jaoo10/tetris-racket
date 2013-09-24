#lang racket
(require htdp/matrix)
(require rackunit)
(require rackunit/text-ui)
(require "tetra-tipos.rkt")
(require "base.rkt")
(require "tetris.rkt")

;; Constantes usadas nos testes
(define TIMEOUT 14)
(define TIMEOUT_NOVO 13)



(define TT1 (tetramino T_TIPOS 1 (posn 1 0) T_COR))
(define TL (tetramino L_TIPOS 2 (posn 0 0) T_COR))
(define TL-MOV (tetramino L_TIPOS 2 (posn 2 2) T_COR))
(define TT1-2 (tetramino T_TIPOS 1 (posn 2 0) T_COR))
(define TT2 (tetramino T_TIPOS 1 (posn 2 2) T_COR))
(define TT3 (tetramino T_TIPOS 3 (posn 1 0) T_COR))
(define TO (tetramino O_TIPOS 0 (posn 1 0) T_COR))
(define TI1 (tetramino I_TIPOS 0 (posn 1 0) T_COR))
(define TI2 (tetramino I_TIPOS 1 (posn 1 0) T_COR))
(define TT1_MOV (tetramino T_TIPOS 1 (posn 1 1) T_COR))
(define TT1_MOV2 (tetramino T_TIPOS 1 (posn 2 0) T_COR))
(define TT1_MOV3 (tetramino T_TIPOS 2 (posn 1 0) T_COR))
(define TT1_MOV4 (tetramino T_TIPOS 0 (posn 1 0) T_COR))
(define TT2_MOV (tetramino T_TIPOS 1 (posn 2 1) T_COR))
(define TT1_POS (list (posn 1 1)
                      (posn 2 1) (posn 2 2)
                      (posn 3 1)))
(define TT1_CENTRA_10 (tetramino T_TIPOS 1 (posn 1 3) T_COR))

(define TZ2 (tetramino Z_TIPOS 2 (posn 2 3) Z_COR))
(define TZ2_POS (list (posn 3 3) (posn 3 4)
                      (posn 4 4) (posn 4 5)))
(define TZ2_CENTRA_15 (tetramino Z_TIPOS 2 (posn 2 6) Z_COR))

(define TI0 (tetramino I_TIPOS 0 (posn -1 1) I_COR))
(define TI0_POS (list (posn 0 1) (posn 0 2) (posn 0 3) (posn 0 4)))
(define TI0_CENTRA_12 (tetramino I_TIPOS 0 (posn -1 4) I_COR))

(define tetras (list TT1 TL TO))
(define tetras2 (list TL TO))

(define C1 (list (list 0 0 0 0 0 0 0)   ; 0
                 (list 0 0 0 0 0 0 0)   ; 1
                 (list 6 0 0 0 0 0 0)   ; 2
                 (list 4 0 2 4 6 1 1)   ; 3
                 (list 3 4 0 0 0 0 0)   ; 4
                 (list 1 2 4 3 2 5 6))) ; 5
                 ;     0 1 2 3 4 5 6

(define C1_LARGURA 7)
(define C1_ALTURA 6)
;; algumas posições ocupadas em C1
(define C1_OCUPADAS (list (posn 2 0) (posn 3 2) (posn 4 1)))
;; algumas posições livres em C1
(define C1_LIVRES (list (posn 0 0) (posn 3 1) (posn 4 2)))

(define C T_COR)

; Representa C1 com o tetraminó TT1 fixado no campo
(define C1_FIXA_TT1 (list (list 0 0 0 0 0 0 0)   ; 0
                          (list 0 C 0 0 0 0 0)   ; 1
                          (list 6 C C 0 0 0 0)   ; 2
                          (list 4 C 2 4 6 1 1)   ; 3
                          (list 3 4 0 0 0 0 0)   ; 4
                          (list 1 2 4 3 2 5 6))) ; 5
                          ;     0 1 2 3 4 5 6

; Representa C1_FIXA_TT1 sem as linha completas
(define C1_FIXA_TT1_LIMPA (list (list 0 0 0 0 0 0 0)   ; 0
                                (list 0 0 0 0 0 0 0)   ; 1
                                (list 0 0 0 0 0 0 0)   ; 2
                                (list 0 C 0 0 0 0 0)   ; 3
                                (list 6 C C 0 0 0 0)   ; 4
                                (list 3 4 0 0 0 0 0))) ; 5
                                ;     0 1 2 3 4 5 6

(define C2 (list (list 0 0 0 0 0)
                 (list 0 0 0 0 0)
                 (list 0 0 0 0 0)
                 (list 0 0 0 0 0)
                 (list 0 0 0 0 0)
                 (list 0 0 0 0 0)
                 (list 0 0 0 0 0)))

(define C2_LARGURA 5)
(define C2_ALTURA 7)

(define trata-tick-tests
  (test-suite
   "trata-tick tests"
   (check-equal? (trata-tick (tetris 
                  C2 
                  C2_LARGURA 
                  C2_ALTURA 
                  TT1 
                  tetras 
                  TIMEOUT))
                 
                 (tetris 
                  C2 
                  C2_LARGURA 
                  C2_ALTURA 
                  TT1 
                  tetras 
                  TIMEOUT_NOVO))
   (check-equal? (trata-tick (tetris 
                  C2 
                  C2_LARGURA 
                  C2_ALTURA 
                  TT1 
                  tetras 
                  0))
                 
                 (tetris 
                  C2 
                  C2_LARGURA 
                  C2_ALTURA 
                  TT1_MOV2 
                  tetras 
                  20))
   ))

(define move-direita-tests
  (test-suite
   "move-direita tests"
   (check-equal? (move-direita (tetris 
                                C2 
                                C2_LARGURA 
                                C2_ALTURA 
                                TT1 
                                empty 
                                TIMEOUT)) 
                 (tetris C2 
                         C2_LARGURA 
                         C2_ALTURA 
                         TT1_MOV 
                         empty 
                         TIMEOUT))))

(define move-esquerda-tests
  (test-suite
   "move-esquerda tests"
   (check-equal? (move-esquerda (tetris 
                                C2 
                                C2_LARGURA 
                                C2_ALTURA 
                                TT2 
                                empty 
                                TIMEOUT)) 
                 (tetris C2 
                         C2_LARGURA 
                         C2_ALTURA 
                         TT2_MOV 
                         empty 
                         TIMEOUT))))

(define move-baixo-tests
  (test-suite
   "move-baixo tests"
   (check-equal? (move-baixo (tetris 
                                C2 
                                C2_LARGURA 
                                C2_ALTURA 
                                TT1 
                                tetras 
                                TIMEOUT)) 
                 (tetris C2 
                         C2_LARGURA 
                         C2_ALTURA 
                         TT1_MOV2 
                         tetras 
                         20))))

(define rotaciona-tests
  (test-suite
   "rotaciona tests"
   (check-equal? (rotaciona (tetris 
                                C2 
                                C2_LARGURA 
                                C2_ALTURA 
                                TT1 
                                empty 
                                TIMEOUT))
                 (tetris 
                                C2 
                                C2_LARGURA 
                                C2_ALTURA 
                                TT1_MOV3 
                                empty 
                                TIMEOUT))
   (check-equal? (rotaciona (tetris 
                                C2 
                                C2_LARGURA 
                                C2_ALTURA 
                                TT3 
                                empty 
                                TIMEOUT))
                 (tetris 
                                C2 
                                C2_LARGURA 
                                C2_ALTURA 
                                TT1_MOV4 
                                empty 
                                TIMEOUT))
   (check-equal? (rotaciona (tetris 
                                C2 
                                C2_LARGURA 
                                C2_ALTURA 
                                TI1 
                                empty 
                                TIMEOUT))
                 (tetris 
                                C2 
                                C2_LARGURA 
                                C2_ALTURA 
                                TI2 
                                empty 
                                TIMEOUT))
   (check-equal? (rotaciona (tetris 
                                C2 
                                C2_LARGURA 
                                C2_ALTURA 
                                TI2 
                                empty 
                                TIMEOUT))
                 (tetris 
                                C2 
                                C2_LARGURA 
                                C2_ALTURA 
                                TI1 
                                empty 
                                TIMEOUT))
   (check-equal? (rotaciona (tetris 
                                C2 
                                C2_LARGURA 
                                C2_ALTURA 
                                TO 
                                empty 
                                TIMEOUT))
                 (tetris 
                                C2 
                                C2_LARGURA 
                                C2_ALTURA 
                                TO 
                                empty 
                                TIMEOUT))))

(define movimenta?-tests
  (test-suite
   "movimenta? tests"
   (check-equal? (movimenta? TT1 (tetris 
                                C2 
                                C2_LARGURA 
                                C2_ALTURA 
                                TT1 
                                empty 
                                TIMEOUT)) true)
   (check-equal? (movimenta? TT1-2 (tetris 
                                C1 
                                C1_LARGURA 
                                C1_ALTURA 
                                TT1 
                                empty 
                                TIMEOUT)) false)
   (check-equal? (movimenta? TL-MOV (tetris 
                                C1 
                                C1_LARGURA 
                                C1_ALTURA 
                                TL 
                                empty 
                                TIMEOUT)) false)))





(define linha-um-tests
  (test-suite
   "fn-linha tests"
   (check-equal? (linha-um '(0 1 0 1 0 1 1) 1 0) (list (posn 1 1) (posn 1 3) (posn 1 5) (posn 1 6))) 
   (check-equal? (linha-um '(1 1 0 1 1) 1 0) (list (posn 1 0) (posn 1 1) (posn 1 3) (posn 1 4)))
   (check-equal? (linha-um empty 0 0) empty)))

(define campo-um-tests
  (test-suite
   "fn-campo tests"
   (check-equal? (campo-um '((0 1 0) (1 0 1) (1 1 1)) 0) (list (posn 0 1) (posn 1 0) (posn 1 2) (posn 2 0) (posn 2 1) (posn 2 2)))
   (check-equal? (campo-um '((0 1 0) (1 1 1) (0 0 0)) 0) (list (posn 0 1) (posn 1 0) (posn 1 1) (posn 1 2)))
   (check-equal? (campo-um empty 0) empty)))

(define incrementa-tests
  (test-suite
   "incrementa tests"
   (check-equal? (incrementa (list (posn 0 1) (posn 1 0) (posn 1 2) (posn 2 0) (posn 2 1) (posn 2 2)) (posn 1 0)) (list (posn 1 1) (posn 2 0) (posn 2 2) (posn 3 0) (posn 3 1) (posn 3 2)))
   (check-equal? (incrementa (list (posn 0 1) (posn 1 0) (posn 1 2) (posn 2 0) (posn 2 1) (posn 2 2)) (posn 0 1)) (list (posn 0 2) (posn 1 1) (posn 1 3) (posn 2 1) (posn 2 2) (posn 2 3)))
   (check-equal? (incrementa (list (posn 0 1) (posn 1 0) (posn 1 2) (posn 2 0) (posn 2 1) (posn 2 2)) (posn 0 0)) (list (posn 0 1) (posn 1 0) (posn 1 2) (posn 2 0) (posn 2 1) (posn 2 2)))
   (check-equal? (incrementa empty (posn 0 1)) empty)))

(define make-linha-tests
  (test-suite
   "make-linha tests"
   (check-equal? (make-linha 0) empty)
   (check-equal? (make-linha 5) (list 0 0 0 0 0))))

(define make-campo-tests
  (test-suite
   "make-campo tests"
   (check-equal? (make-campo C2_LARGURA C2_ALTURA) C2)))

(define centraliza-tests
  (test-suite
   "centraliza tests"
   (check-equal? (centraliza TT1 10)
                 TT1_CENTRA_10)
   (check-equal? (centraliza TZ2 15)
                 TZ2_CENTRA_15)
   (check-equal? (centraliza TI0 12)
                 TI0_CENTRA_12)))

(define make-tetris-tests
  (test-suite
   "make-tetris tests"
   (check-equal? (make-tetris C2_LARGURA C2_ALTURA (list TT1 TZ2 TI0) TIMEOUT)
                 (tetris C2
                         C2_LARGURA
                         C2_ALTURA
                         (centraliza TT1 C2_LARGURA)
                         (list TZ2 TI0)
                         TIMEOUT))))

(define tetramino->pos-tests
  (test-suite
   "tetramino->pos tests"
   (check-equal? (tetramino->lista-pos TT1) TT1_POS)
   (check-equal? (tetramino->lista-pos TZ2) TZ2_POS)
   (check-equal? (tetramino->lista-pos TI0) TI0_POS)))

(define lop-validas?-tests
  (test-suite
   "lop-validas? tests"
   (check-equal? (lop-validas? empty 5 8)
                 #t)
   ;; testa os extremos
   (check-equal? (lop-validas? (list (posn 0 0)
                                     (posn (sub1 C1_ALTURA) 0)
                                     (posn 0 (sub1 C1_LARGURA))
                                     (posn (sub1 C1_ALTURA) (sub1 C1_LARGURA)))
                               C1_LARGURA
                               C1_ALTURA)
                 #t)
   (check-equal? (lop-validas? (list (posn 0 0)
                                     (posn C1_ALTURA 0) ; linha inválida
                                     (posn 1 2))
                               C1_LARGURA
                               C1_ALTURA)
                 #f)
   (check-equal? (lop-validas? (list (posn  2 3)
                                     (posn -1 3)) ; linha inválida
                               C1_LARGURA
                               C1_ALTURA)
                 #f)
   (check-equal? (lop-validas? (list (posn 0 0)
                                     (posn 0 C1_LARGURA) ; coluna inválida
                                     (posn 1 2))
                               C1_LARGURA
                               C1_ALTURA)
                 #f)
   (check-equal? (lop-validas? (list (posn 0 0)
                                     (posn 1 -1)) ; coluna inválida
                               C1_LARGURA
                               C1_ALTURA)
                 #f)))

(define lop-livres?-tests
  (test-suite
   "lop-livres? tests"
   (check-equal? (lop-livres? C1_LIVRES C1) #t)
   (check-equal? (lop-livres? C1_OCUPADAS C1) #f)
   (check-equal? (lop-livres? (append C1_LIVRES (list (first C1_OCUPADAS))) C1) #f)))

(define linha-completa-tests
  (test-suite
   "linha-completa? tests"
   (check-equal? (linha-completa? '(1 0 0 0 1 0)) #f)
   (check-equal? (linha-completa? '(1 1 1 1 1 1)) #t)
   (check-equal? (linha-completa? empty) #t)))

(define campo-completas-tests
  (test-suite
   "campo-completas tests"
   (check-equal? (campo-completas C1) (list (list 0 0 0 0 0 0 0)   ; 0
                                            (list 0 0 0 0 0 0 0)   ; 1
                                            (list 6 0 0 0 0 0 0)   ; 2
                                            (list 4 0 2 4 6 1 1)   ; 3
                                            (list 3 4 0 0 0 0 0))) ; 4
                                            ;     0 1 2 3 4 5 6)
   (check-equal? (campo-completas C2) C2)
   (check-equal? (campo-completas empty) empty)))


(define fixa-tests
  (test-suite
   "fixa tests"
   (check-equal? (fixa (tetris C1 C1_LARGURA C1_ALTURA TT1 empty TIMEOUT))
                 (tetris C1_FIXA_TT1 C1_LARGURA C1_ALTURA TT1 empty TIMEOUT))))

(define limpa-tests
  (test-suite
   "limpa tests"
   (check-equal? (limpa (tetris C1_FIXA_TT1 C1_LARGURA C1_ALTURA TT1 empty TIMEOUT))
                 (tetris C1_FIXA_TT1_LIMPA C1_LARGURA C1_ALTURA TT1 empty TIMEOUT))))

;; ---------------------------------------------------------------------

;; Função que executa um grupo de testes.
(define (executar-testes . testes)
  (run-tests (test-suite "Todos os testes" testes))
  (void))

;; Chama a função para executar os testes.

(executar-testes make-linha-tests
                 make-campo-tests
                 centraliza-tests
                 make-tetris-tests
                 tetramino->pos-tests
                 lop-validas?-tests
                 lop-livres?-tests
                 fixa-tests
                 limpa-tests
                 linha-um-tests
                 campo-um-tests
                 incrementa-tests
                 linha-completa-tests
                 campo-completas-tests
                 move-direita-tests
                 move-esquerda-tests
                 move-baixo-tests
                 rotaciona-tests
                 movimenta?-tests
                 trata-tick-tests)
                 
