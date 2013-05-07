#lang racket

(require (only-in "../cfa2/cfa2.rkt" BP)
         (only-in "../pda-to-pda-risc/risc-enhanced/data.rkt"
                  get-uid pda-term-insn)
         "push-pop-webs.rkt")
(provide summaries->webset)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing Set-Up
(module+ test
  (require errortrace rackunit "../pda-to-pda-risc/risc-enhanced/data.rkt")
  (define (dummy-push uid)
    (uninitialized-pda-term (push uid (curr-token #f))))
  (define (dummy-pop uid)
    (uninitialized-pda-term (assign uid (uninitialized-register 'foo) (pop)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code

;; BP->pair-of-uid : [BP Term Term] -> Number Number
;;
;; strips a BP down to a pair of UIDs
(define (BP->pair-of-uid bp)
  (match-let (((BP a b) bp))
    (list (get-uid (pda-term-insn a))
          (get-uid (pda-term-insn b)))))
(module+ test
  (check-equal? (BP->pair-of-uid (BP (dummy-push 1) (dummy-pop 2)))
                (list 1 2)))

;; summaries->webset : [Set [BP Term Term]] -> [Webset Number Number]
;;
;; generates a webset with the UIDs of the pushes and pops in the summaries set.
(define (summaries->webset summaries)
  (webset-from-relation (set-map summaries BP->pair-of-uid)))
(module+ test
  (check-equal? (summaries->webset (set (BP (dummy-push 1) (dummy-pop 2))
                                        (BP (dummy-push 3) (dummy-pop 4))
                                        (BP (dummy-push 1) (dummy-pop 4))))
                (set (web (set 1 3) (set 2 4))))
  (check-equal? (summaries->webset (set (BP (dummy-push 1) (dummy-pop 2))
                                        (BP (dummy-push 3) (dummy-pop 4))
                                        (BP (dummy-push 1) (dummy-pop 6))))
                (set (web (set 1) (set 2 6))
                     (web (set 3) (set 4)))))
