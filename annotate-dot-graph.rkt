#lang racket

(require "make-dot-graph.rkt"
         "dot-graph-data.rkt"
         "push-pop-webs.rkt")
(provide add-summary-edges add-webset-edges add-webset-node-colorings)

(module+ test (require rackunit))

(define default-summary-edge-attributes
  (hash 'style "dashed" 'color "red"))
(define default-web-edge-attributes
  (hash 'style "dotted" 'color "blue" 'dir "none" 'constraint "false"))

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
    (add-node g (uid->node-name uid) (hash 'color color))))

;; generate-colors : Natural -> [ListOf String]
;;
;; produces n RGB color strings
(define (generate-colors n)
  (let* ((divisons-per-axis (exact-floor (/ n 3)))
         (extra-divisons (modulo n 3))
         (red-divisions (+ divisons-per-axis (if (> extra-divisons 0) 1 0)))
         (green-divisions (+ divisons-per-axis (if (> extra-divisons 1) 1 0)))
         (blue-divisions (+ divisons-per-axis (if (> extra-divisons 2) 1 0))))
    (append (map (curry one-axis-rgb->string 0)
                 (partition-255 red-divisions))
            (map (curry one-axis-rgb->string 1)
                 (partition-255 green-divisions))
            (map (curry one-axis-rgb->string 2)
                 (partition-255 blue-divisions)))))

(module+ test
  (check-equal? (generate-colors 0) empty)
  (check-equal? (generate-colors 1) (list "255 0 0"))
  (check-equal? (generate-colors 2) (list "255 0 0"
                                          "0 255 0"))
  (check-equal? (generate-colors 3) (list "255 0 0"
                                          "0 255 0"
                                          "0 0 255"))
  (check-equal? (generate-colors 4) (list "127 0 0"
                                          "255 0 0"
                                          "0 255 0"
                                          "0 0 255"))
  (check-equal? (generate-colors 5) (list "127 0 0"
                                          "255 0 0"
                                          "0 127 0"
                                          "0 255 0"
                                          "0 0 255"))
  (check-equal? (generate-colors 6) (list "127 0 0"
                                          "255 0 0"
                                          "0 127 0"
                                          "0 255 0"
                                          "0 0 127"
                                          "0 0 255")))

;; one-axis-rgb->string : Natural Natural -> [ListOf String]
;;
;; Given an axis value and an axis index, produce a dot-compatible RGB string
(define (one-axis-rgb->string axis value)
  (cond [(= axis 0) (string-append (number->string value) " 0 0")]
        [(= axis 1) (string-append "0 " (number->string value) " 0")]
        [(= axis 2) (string-append "0 0 " (number->string value))]))

(module+ test
  (check-equal? (one-axis-rgb->string 0 255 ) "255 0 0")
  (check-equal? (one-axis-rgb->string 1 28) "0 28 0")
  (check-equal? (one-axis-rgb->string 2 127) "0 0 127"))

;; partition-255 : Natural -> [ListOf Natural]
;;
;; Partition the [0,255] number range into equal intervals. The result list
;; (list A B C ...) represents the intervals [0, A], [A+1, B], [B+1, C], ...
(define (partition-255 n)
  (build-list n (lambda (i) (exact-floor (* (/ 255 n) (add1 i))))))

(module+ test
  (check-equal? (partition-255 0) empty)
  (check-equal? (partition-255 1) (list 255))
  (check-equal? (partition-255 2) (list 127 255))
  (check-equal? (partition-255 3) (list 85 170 255))
  (check-equal? (partition-255 4) (list 63 127 191 255)))

