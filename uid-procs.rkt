#lang racket

(require (only-in "../cfa2/cfa2.rkt" BP)
         (only-in "../pda-to-pda-risc/risc-enhanced/data.rkt"
                  get-uid pda-term-insn unparse)
         "push-pop-webs.rkt")

(provide make-uid->term/hash make-uid->fv/hash
         BP->pair-of-uid uid-webset->unparsed-webset)

(define (BP->pair-of-uid bp)
  (match-let (((BP a b) bp))
    (list (get-uid (pda-term-insn a))
          (get-uid (pda-term-insn b)))))

(define (make-uid->term/hash node->fv/hash)
  (for/hash (((k _) (in-hash node->fv/hash)))
    (values (get-uid (pda-term-insn k)) k)))

(define (make-uid->fv/hash node->fv/hash)
  (for/hash (((k v) (in-hash node->fv/hash)))
    (values (get-uid (pda-term-insn k)) v)))

(define (uid-webset->unparsed-webset uid->term webset)
  (for/set ((w webset))
    (web (for/set ((push (web-pushes w)))
           (unparse (uid->term push)))
         (for/set ((pop (web-pops w)))
           (unparse (uid->term pop))))))
