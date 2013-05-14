#lang racket

(require "push-pop-webs.rkt"
         "summaries-to-webset.rkt"
         "useless-stack-ensures.rkt"
         "uid-procs.rkt"

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
;;                  [Web UID UID]
;;                  [ListOf Term]
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

;; standard-overview : [CFA2-Results FV]
;;                     ->
;;                     [Results-Summary FV]
(define (standard-overview cfa2-results)
  (match-define (list node->fv/hash summaries callers pre) cfa2-results)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; uid hashes, procs, and summaries

  (define uid->fv/hash (make-uid->fv/hash node->fv/hash))
  (define uid->term/hash (make-uid->term/hash node->fv/hash))

  (define uid->fv (curry hash-ref uid->fv/hash))
  (define uid->term (curry hash-ref uid->term/hash))

  (define uid-summaries (set-map summaries BP->pair-of-uid))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; push pop web
  (printf "generating push pop web ...\n") (flush-output)

  (define push-pop-web/uid
    (time (webset-from-relation uid-summaries)))
  (define push-pop-web/readable
    (time (uid-webset->unparsed-webset uid->term push-pop-web/uid)))

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

  (printf "=> Push Pop Web\n")
  (pretty-print push-pop-web/readable)
  (printf "=> Number of paths: ~a\n" (hash-count node->fv/hash))
  (printf "=> Number of summaries: ~a\n" (set-count summaries))
  (printf "=> Number of callers: ~a\n" (set-count callers))
  (printf "=> Number of webs: ~a\n" (set-count push-pop-web/uid))
  (printf "=> Number of reachable pushes: ~a\n" (length pushes/term))
  (printf "=> Number of reachable pops: ~a\n" (length pop-assigns/term))
  (printf "=> Number of reachable stack ensures: ~a\n" (length stack-ensures/term))
  (printf "=> Number of useless stack ensures: ~a\n" (length useless-ensures))

  (results-summary uid->fv/hash
                   uid->term/hash
                   uid-summaries
                   ;; analysis results
                   push-pop-web/uid
                   useless-ensures
                   ;; modified pdarisc and specific terms
                   pre
                   pda-risc/se
                   pushes/term
                   pop-assigns/term))
