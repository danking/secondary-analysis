#lang racket

(require "../pda-to-pda-risc/risc-enhanced/search.rkt"
         "../pda-to-pda-risc/risc-enhanced/data.rkt"
         "dot-graph-data.rkt")
(provide make-dot-graph)

(define (make-dot-graph pre)
  (define (combine t g)
    (let* ((g (add-term-node g t))
           (succs (pda-term-succs t)))
      (add-succ-edges g t succs)))

  (folding-search combine empty-digraph (pda-risc-enh-initial-term pre)))

(define (shorten s)
  (substring s 0 (min 100 (string-length s))))

(define (textify t)
  (shorten (term->string t)))

;; uses the risc-enhnaced/data.rkt unparser
(define term->string
  (compose pretty-format pre-term->risc-sexp))

(define (term->node-name t)
  (string->symbol
   (string-append "id" (number->string (get-uid (pda-term-insn t))))))

(define (add-term-node g t)
  (let ((text (textify t))
        (name (term->node-name t)))
    (add-node g name (hash 'label text))))

(define (add-succ-edges g t succs)
  (for/fold
      ((g g))
      ((succ succs))
    (add-edge g
              (term->node-name t)
              (term->node-name succ)
              (hash))))
