#lang racket

;; Este arquivo contém a chamada inicial do programa. Não é necessário editar
;; este arquivo. Se for necessário mudar o jogo inicial, altere a função
;; make-tetris-padrao no arquivo tetris.rkt.

(require "tetris.rkt")
(require 2htdp/universe)

(big-bang (make-tetris-padrao)
          (on-key trata-tecla)
          (on-tick trata-tick)
          (on-draw desenha))
