#lang racket

(require "push-pop-webs.rkt")

(provide find-missing-pushes find-missing-pops)

;; find-missing-pushes : [SetOf [List UID UID]] [ListOf UID] -> [SetOf UID]
;;
;; returns the set of pushes which do not appear in the summaries set
(define (find-missing-pushes uid-summaries pushes)
  (for/fold
      ((pushes-set (list->set pushes)))
      ((pair uid-summaries))
    (set-remove pushes-set (first pair))))

;; find-missing-pushes : [SetOf [List UID UID]] [ListOf UID] -> [SetOf UID]
;;
;; returns the set of pops which do not appear in the summaries set
(define (find-missing-pops uid-summaries pops)
  (for/fold
      ((pops-set (list->set pops)))
      ((pair uid-summaries))
    (set-remove pops-set (second pair))))
