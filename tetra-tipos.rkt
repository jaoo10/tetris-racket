#lang racket

(provide I_TIPOS I0 I1
         J_TIPOS J0 J1 J2 J3
         L_TIPOS L0 L1 L2 L3
         O_TIPOS O0
         S_TIPOS S0 S1 S2 S3
         T_TIPOS T0 T1 T2 T3
         Z_TIPOS Z0 Z1 Z2 Z3)
         

(define I0 '((0 0 0 0)
             (1 1 1 1)
             (0 0 0 0)
             (0 0 0 0)))

(define I1 '((0 1 0 0)
             (0 1 0 0)
             (0 1 0 0)
             (0 1 0 0)))

(define J0 '((1 0 0)
             (1 1 1)
             (0 0 0)))

(define J1 '((0 1 1)
             (0 1 0)
             (0 1 0)))

(define J2 '((0 0 0)
             (1 1 1)
             (0 0 1)))

(define J3 '((0 1 0)
             (0 1 0)
             (1 1 0)))

(define L0 '((0 0 1)
             (1 1 1)
             (0 0 0)))

(define L1 '((0 1 0)
             (0 1 0)
             (0 1 1)))

(define L2 '((0 0 0)
             (1 1 1)
             (1 0 0)))

(define L3 '((1 1 0)
             (0 1 0)
             (0 1 0)))

(define O0 '((1 1 0)
             (1 1 0)
             (0 0 0)))

(define S0 '((0 1 1)
             (1 1 0)
             (0 0 0)))
             
(define S1 '((0 1 0)
             (0 1 1)
             (0 0 1)))

(define S2 '((0 0 0)
             (0 1 1)
             (1 1 0)))

(define S3 '((1 0 0)
             (1 1 0)
             (0 1 0)))

(define T0 '((0 1 0)
             (1 1 1)
             (0 0 0)))

(define T1 '((0 1 0)
             (0 1 1)
             (0 1 0)))

(define T2 '((0 0 0)
             (1 1 1)
             (0 1 0)))

(define T3 '((0 1 0)
             (1 1 0)
             (0 1 0)))

(define Z0 '((1 1 0)
             (0 1 1)
             (0 0 0)))

(define Z1 '((0 0 1)
             (0 1 1)
             (0 1 0)))

(define Z2 '((0 0 0)
             (1 1 0)
             (0 1 1)))

(define Z3 '((0 1 0)
             (1 1 0)
             (1 0 0)))

(define I_TIPOS (list I0 I1))
(define J_TIPOS (list J0 J1 J2 J3))
(define L_TIPOS (list L0 L1 L2 L3))
(define O_TIPOS (list O0))
(define S_TIPOS (list S0 S1 S2 S3))
(define T_TIPOS (list T0 T1 T2 T3))
(define Z_TIPOS (list Z0 Z1 Z2 Z3))
