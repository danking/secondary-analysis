#lang racket

(require "make-dot-graph.rkt"
         "dot-graph-data.rkt"
         "push-pop-webs.rkt")
(provide add-summary-edges add-webset-edges)

(define default-summary-edge-attributes
  (hash 'style "dashed" 'color "red"))
(define default-web-edge-attributes
  (hash 'style "dotted" 'color "blue" 'dir "none"))

;; add-summary-edges : [SetOf [List UID UID]] Digraph -> Digraph
;;
;; adds the edges from a UID summary set
(define (add-summary-edges summaries g)
  (for/fold
      ((g g))
      ((sum summaries))
    (add-uid-edge (first sum) (second sum) g)))

;; add-uid-edge : UID UID Digraph -> Digraph
;;
;; Produces a new digraph with an edge pointing from the first uid to the second
;; one.
(define (add-uid-edge from to g)
  (add-edge g (uid->node-name from) (uid->node-name to)
            default-summary-edge-attributes))

;; add-webset-edges : [SetOf [Web UID UID]] Digraph -> Digraph
;;
;; Adds the edges implied by each web from the webset to the digraph. These
;; edges enable the viewer to easily see interrelationships between push and pop
;; nodes.
(define (add-webset-edges webset g)
  (for/fold
      ((g g))
      ((web webset))
    (add-web-edges web g)))

;; add-web-edges : [Web UID UID] Digraph -> Digraph
;;
;; Adds an edge from every push to every pop and every pop to every push
(define (add-web-edges web g)
  (for*/fold
      ((g g))
      ((from (web-pushes web))
       (to (web-pops web)))
    (add-web-edge from to g)))

;; add-web-edge : UID UID Digraph -> Digraph
;;
;; Adds an edge from the first UID to the second
(define (add-web-edge from to g)
  (add-edge g (uid->node-name from) (uid->node-name to)
            default-web-edge-attributes))
