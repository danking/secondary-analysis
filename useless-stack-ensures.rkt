#lang racket

(require "../pda-to-pda-risc/risc-enhanced/search.rkt"
         "../pda-to-pda-risc/risc-enhanced/data.rkt"
         "../cfa2-analyses/min-headroom.rkt"
         "../lattice/lattice.rkt"
         )
(provide useless-stack-ensures)

(define headroom-gte? (lattice-gte? min-headroom-bounded-lattice))

(define (useless-stack-ensures pre uid->min-hdrm)
  (define (combine t useless-ensures)
    (let ((i (pda-term-insn t)))
      (if (and (stack-ensure? i)
               (headroom-gte? (uid->min-hdrm (get-uid i))
                              (stack-ensure-hdrm i)))
          (cons t useless-ensures)
          useless-ensures)))

  (folding-forward-search combine empty (pda-risc-enh-initial-term pre)))
