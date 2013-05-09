#lang racket

(require "../pda-to-pda-risc/risc-enhanced/search.rkt"
         "../pda-to-pda-risc/risc-enhanced/data.rkt"
         "dot-graph-data.rkt")
(provide make-dot-graph)

;; make-dot-graph : PDA-RISC -> Digraph
;;
;; Produces a visually appealing Graphviz dot-langauge compatible Digraph from a
;; pda-risc term by traversing the graph.
(define (make-dot-graph pre)
  ;; combine : Term Digraph -> Digraph
  ;;
  ;; Combine only adds the current term to the node set, but adds all the
  ;; outgoing edges to the edge set. We don't add all the successors themselves
  ;; to the node set now because they will be proccessed later.
  (define (combine t g)
    (let* ((g (add-term-node g t))
           (succs (pda-term-succs t)))
      (add-valid-succ-edges g t succs)))

  (let ((empty-graph-with-settings
         (set-global-node-attribute empty-digraph
                                    'fontname
                                    "monospace")))
    (folding-search combine
                    empty-graph-with-settings
                    (pda-risc-enh-initial-term pre))))

;; make-8.5x11-graph : PDA-RISC -> Digraph
;;
;; Produces a digraph which will fit on one US letter page if using the
;; Postscript output of dot
(define (make-8.5x11-graph pre)
  (set-attribute (make-dot-graph pre)
                 'page
                 "8.5,11"))

;; add-valid-succ-edges : Digraph Term [SetOf Term] -> Digraph
;;
;; Add to the digraph a representation of the terms in succs that are
;; `valid-term?'. The representing nodes' names are defined by
;; `term->node-name'. Their "styling", i.e. their label and color, is defined by
;; `add-term-node'.
(define (add-valid-succ-edges g t succs)
  (for/fold
      ((g g))
      ((succ succs)
       #:when (valid-term? succ))
    (add-edge g
              (term->node-name t)
              (term->node-name succ)
              (hash))))

;; valid-term? : Term -> Boolean
;;
;; A term is considered `valid' if its printed form has necessary information
;; for understanding the pda. In the case of block and block* this is
;; untrue.
(define (valid-term? term)
  (let ((i (pda-term-insn term)))
    (not (or (block? i)
             (block*? i)
             (label? i)))))

;; string->left-aligned-string : String -> String
;;
;; Dot has three different escape sequences for newline. It uses \n, \l, and
;; \r. These create center-aligned, left-aligned, and right-aligned lines,
;; respectively.
(define (string->left-aligned-string s)
  (string-replace s "\n" "\\l"))

;; term->string : Term -> String
;;
;; Converts a term into a string for dot labels by producing a s-expression,
;; then using the pretty-printer to produce a string and then converts the
;; string to a left-aligned string (by dot convetions).
;;
;; uses the risc-enhnaced/data.rkt unparser
(define term->string
  (compose string->left-aligned-string
           pretty-format
           pre-term->risc-sexp))

(define (term->node-name t)
  (string->symbol
   (string-append "id" (number->string (get-uid (pda-term-insn t))))))

(define (add-term-node g t)
  (let ((text (textify t))
        (name (term->node-name t)))
    (add-node g name (hash 'label text))))

(define (shorten s)
  (substring s 0 (min 100 (string-length s))))

(define (textify t)
  (shorten (term->string t)))

