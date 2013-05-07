#lang racket

(require "../pda-to-pda-risc/risc-enhanced/search.rkt"
         "../pda-to-pda-risc/risc-enhanced/data.rkt")
(provide useless-stack-ensures)

(define (useless-stack-ensures pre uid->min-hdrm)
  (define (combine t useless-ensures)
    (let ((i (pda-term-insn t)))
      (if (or (not (stack-ensure? i))
              (> (stack-ensure-hdrm i)
                 (uid->min-hdrm (get-uid i))))
          useless-ensures
          (cons t useless-ensures))))

  (folding-search combine empty (pda-risc-enh-initial-term pre)))
