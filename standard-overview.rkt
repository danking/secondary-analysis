#lang racket

(require "push-pop-webs.rkt"
         "summaries-to-webset.rkt"
         "useless-stack-ensures.rkt"
         "uid-procs.rkt"
         "term-results.rkt"

         "../pda-to-pda-risc/risc-enhanced/search.rkt"

         (only-in "../pda-to-pda-risc/risc-enhanced/data.rkt"
                  pda-term-insn
                  pda-risc-enh-initial-term
                  pda-risc-enh->pda-risc
                  push?
                  stack-ensure?
                  pop-assign?))
(provide standard-overview
         (struct-out results-summary))

;; A [Results-Summary FV] is a
;; (results-summary [UID -> FV]
;;                  [UID -> Term]
;;                  [SetOf [List UID UID]]
;;                  [Web UID UID]
;;                  [ListOf Term]
;;                  PDA-RISC
;;                  PDA-RISC
;;                  [ListOf Term]
;;                  [ListOf Term]
;;
;; In particular,
;;   useless-ensures is a list of stack-ensure terms
;;   pushes is a list of push terms
;;   pops is a list of pop terms
(struct results-summary
        (uid->fv/hash
         uid->term/hash
         uid-summaries
         ;; analysis results
         push-pop-web
         useless-ensures
         ;; modified pdarisc and specific terms
         pda-risc-enh/se
         pda-risc/se
         pushes
         pops))

;; I edited this file mostly with the intention of creating a straightforward
;; way to get the standard statistics on the cfa2 results. It should work, but
;; often times the defined identifiers are actually nice to have on hand.

;; standard-overview : Term-Resutls
;;                     PDA-RISC-ENH
;;                     String
;;                     ->
;;                     [Results-Summary FV]
(define (standard-overview term-results pre logfile)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; uid hashes, procs, and summaries

  (define uid->fv/hash (term-results-uid->fv/hash term-results))
  (define uid->term/hash (term-results-uid->term/hash term-results))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; push pop web
  (printf "generating push pop web ...\n") (flush-output)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; useless stack ensures
  (printf "counting useless stack-ensures ...\n") (flush-output)

  (define useless-ensures
    (time (useless-stack-ensures pre (lambda (uid)
                                       (hash-ref uid->fv/hash uid 0)))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; pda-risc w/ stack ensures
  (printf "converting back to a pda-risc term ...\n") (flush-output)

  (define pda-risc/se
    (pda-risc-enh->pda-risc pre))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; collect reachable pushes
  (printf "collecting reachable pushes ...\n") (flush-output)

  (define pushes/term
    (time (folding-forward-search (lambda (t pushes)
                                    (if (push? (pda-term-insn t))
                                        (cons t pushes)
                                        pushes))
                                  empty
                                  (pda-risc-enh-initial-term pre))))

  (define pushes/insn
    (for/set ((t pushes/term))
      (pda-term-insn t)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; collect reachable stack ensures
  (printf "collecting reachable stack ensures ...\n") (flush-output)

  (define stack-ensures/term
    (time (folding-forward-search (lambda (t pushes)
                                    (if (stack-ensure? (pda-term-insn t))
                                        (cons t pushes)
                                        pushes))
                                  empty
                                  (pda-risc-enh-initial-term pre))))

  (define stack-ensures/insn
    (for/set ((t stack-ensures/term))
      (pda-term-insn t)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; collect reachable pops
  (printf "collecting reachable pops ...\n") (flush-output)

  (define pop-assigns/term
    (time (folding-forward-search (lambda (t pushes)
                                    (if (pop-assign? (pda-term-insn t))
                                        (cons t pushes)
                                        pushes))
                                  empty
                                  (pda-risc-enh-initial-term pre))))

  (define pop-assigns/insn
    (for/set ((t stack-ensures/term))
      (pda-term-insn t)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; statistics

  (define output
    `((push-pop-web ,(term-results-push-pop-web term-results))
      (summaries ,(set-count (term-results-summaries term-results)))
      (callers ,(set-count (term-results-callers term-results)))
      (webs ,(set-count (term-results-push-pop-web term-results)))
      (reachable-pushes ,(length pushes/term))
      (reachable-pops ,(length pop-assigns/term))
      (reachable-stack-ensures ,(length stack-ensures/term))
      (useless-stack-ensures ,(length useless-ensures))))

  (with-output-to-file logfile
    (lambda () (pretty-print output)))

  (results-summary uid->fv/hash
                   uid->term/hash
                   (term-results-summaries term-results)
                   ;; analysis results
                   (term-results-push-pop-web term-results)
                   useless-ensures
                   ;; modified pdarisc and specific terms
                   pre
                   pda-risc/se
                   pushes/term
                   pop-assigns/term))
