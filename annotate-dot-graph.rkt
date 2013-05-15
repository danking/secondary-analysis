#lang racket

(require "make-dot-graph.rkt"
         "dot-graph-data.rkt"
         "push-pop-webs.rkt"
         srfi/13)
(provide add-summary-edges add-webset-edges add-webset-node-colorings)

(module+ test (require rackunit))

(define default-summary-edge-attributes
  (hash 'style "dashed" 'color "red"))
(define default-web-edge-attributes
  (hash 'style "dotted" 'color "blue" 'dir "none" 'constraint 'false))

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

;; add-webset-node-colorings : [SetOf [Web UID UID]] Digraph -> Digraph
;;
;; Colors the nodes in each web the same color.
(define (add-webset-node-colorings webset g)
  (for/fold
      ((g g))
      ((web webset)
       (color (generate-colors (set-count webset))))
    (add-nodes-with-color (set-union (web-pushes web) (web-pops web))
                          color
                          g)))

(define (add-nodes-with-color nodes color g)
  (for/fold
      ((g g))
      ((uid nodes))
    (add-node g (uid->node-name uid) (hash 'style "filled"
                                           'fillcolor color))))

;; generate-colors : Natural -> [ListOf String]
;;
;; produces n RGB color strings
(define (generate-colors n)
  (for/list ((color (partition-color-space n)))
    (rgb-list->rgb-hex-string color)))

(define MINIMUM_COLOR_VALUE 25)
(define MAXIMUM_COLOR_VALUE 230)

;; rgb-list->rgb-hex-string : [List Natural Natural Natural] -> String
;;
;; Converts a list representing a vector in RGB space into the corresponding
;; rgb hex code string.
(define (rgb-list->rgb-hex-string ls)
  (string-append "#"
                 (string-pad (number->string (first ls) 16) 2 #\0)
                 (string-pad (number->string (second ls) 16) 2 #\0)
                 (string-pad (number->string (third ls) 16) 2 #\0)))

(module+ test
  (check-equal? (rgb-list->rgb-hex-string '(255 255 255)) "#ffffff")
  (check-equal? (rgb-list->rgb-hex-string '(255 0 255)) "#ff00ff")
  (check-equal? (rgb-list->rgb-hex-string '(255 255 0)) "#ffff00")
  (check-equal? (rgb-list->rgb-hex-string '(127 127 0)) "#7f7f00")
  (check-equal? (rgb-list->rgb-hex-string '(32 32 32)) "#202020"))

;; partition-color-space : Natural -> [SetOf [ListOf Natural]]
;;
;; Partitions RGB color space into total-blocks number of partitions.
(define (partition-color-space total-blocks)
  (let ((splits-per-side (exact-ceiling (expt total-blocks 1/3))))
    (partition-hyper-cube MINIMUM_COLOR_VALUE
                          MAXIMUM_COLOR_VALUE
                          splits-per-side
                          3)))

;; patition-hyper-cube : Natural
;;                       Natural
;;                       Natural
;;                       Natural
;;                       ->
;;                       [SetOf [ListOf Natural]]
;;
;; Parition a dims-dimensional hyper-cube positioned at (min, min, min, ...)
;; with a side length of (- max min) into splits^3 equal-sized hyper-cubes.
(define (partition-hyper-cube min max splits dims)
  (if (= dims 0)
      (set empty)
      (let* ((range (- max min))
             (step (/ range splits))
             (tails (partition-hyper-cube min max splits (sub1 dims))))
        (for*/set ((i (in-range 0 splits))
                   (tail tails))
          (let ((divider (+ min (exact-floor (* (add1 i) step)))))
            (cons divider tail))))))

(module+ test
  (check-equal? (partition-hyper-cube 0 10 2 0)
                (set '()))
  (check-equal? (partition-hyper-cube 0 10 2 1)
                (set '(5) '(10)))
  (check-equal? (partition-hyper-cube 4 10 2 1)
                (set '(7) '(10)))
  (check-equal? (partition-hyper-cube 0 10 2 2)
                (set '(5 5) '(5 10) '(10 5) '(10 10))))
