#lang racket

(provide empty-digraph named-empty-digraph
         set-attribute add-edge add-node digraph->string)

(module+ test (require rackunit))

;; a [Digraph X] is a (digraph Symbol
;;                             [SetOf [Edge X]]
;;                             [SetOf [Node X]]
;;                             [SetOf [Digraph X]]
;;                             Attribute-Hash)
(struct digraph (name edge-set node-set sub-graphs attributes))
(define empty-digraph (digraph "G" (set) (set) (set) (hash)))
(define (named-empty-digraph name) (digraph name (set) (set) (set) (hash)))

;; An [Edge X] is a (edge X X Attribute-Hash)
(struct edge (from to attributes))

;; An Attribute-Hash is a [Hash String (U String Number Symbol)]

;; A [Node X] is a (node Symbol Attribute-Hash)
(struct node (name attributes))

(define (set-attribute g attribute value)
  (match g
    ((digraph name es ns sgs attributes)
     (digraph name es ns sgs (hash-set attributes attribute value)))))

(define (add-edge g from to [attributes (hash)])
  (match g
    ((digraph name es ns sgs attr)
     (digraph name (set-add es (edge from to attributes)) ns sgs attr))))

(define (add-node g node-name [attributes (hash)])
  (match g
    ((digraph graph-name es ns sgs attr)
     (digraph graph-name es (set-add ns (node node-name attributes))
              sgs attr))))

(define (add-subgraph g sg)
  (match g
    ((digraph name es ns sgs attr)
     (digraph name es ns (set-add sgs sg) attr))))

(define (digraph->string g [indent ""])
  (graph->string g "digraph" indent))
(module+ test
  (check-equal? (digraph->string (add-node (add-edge empty-digraph
                                                     'foo 'bar)
                                           'foo (hash 'color "0 0 255")))
"digraph G {
  foo [color = \"0 0 255\", ];
  foo -> bar [];
}\n")
  (check-equal? (digraph->string
                 (add-subgraph (add-node (add-edge empty-digraph
                                                   'foo 'bar)
                                         'foo (hash 'color "0 0 255"))
                               (add-edge (named-empty-digraph "cluster")
                                         'baz 'qux)))
"digraph G {
  subgraph cluster {
    baz -> qux [];
  }

  foo [color = \"0 0 255\", ];
  foo -> bar [];
}\n"))

(define (graph->string g keyword indent)
  (match g
    ((digraph name es ns sgs attr)
     (string-append indent "digraph " name " {\n"
                    (for/fold
                        ((s ""))
                        (((k v) attr))
                      (string-append s
                                     indent "  "
                                     (attribute->string k v)
                                     ";\n"))
                    (for/fold
                        ((s ""))
                        ((sg sgs))
                      (string-append s
                                     indent
                                     (graph->string sg
                                                    "subgraph"
                                                    (string-append "  " indent))
                                     "\n"))
                    (for/fold
                        ((s ""))
                        ((n ns))

                      (string-append s indent "  " (node->string n) ";\n"))
                    (for/fold
                        ((s ""))
                        ((e es))
                      (string-append s indent "  " (edge->string e) ";\n"))
                    "}\n"))))


(define (attribute->string k v)
  (string-append (symbol->string k) " = " (outputify v)))
(module+ test
  (check-equal? (attribute->string 'size "4,4") "size = \"4,4\"")
  (check-equal? (attribute->string 'weight 3) "weight = 3"))

(define (attributes->attributes-list attributes)
  (string-append "["
                 (for/fold ((s "")) (((k v) attributes))
                   (string-append (attribute->string k v) ", " s))
                 "]"))
(module+ test
  (check-equal? (attributes->attributes-list (hash))
                "[]")
  (check-equal? (attributes->attributes-list (hash 'color "0 0 255"
                                                   'size "4,4"))
                "[size = \"4,4\", color = \"0 0 255\", ]"))

(define (node->string n)
  (match n
    ((node name attributes)
     (string-append (symbol->string name) " "
                    (attributes->attributes-list attributes)))))
(module+ test
  (check-equal? (node->string (node 'foo (hash)))
                "foo []")
  (check-equal? (node->string (node 'foo (hash 'color "0 0 255")))
                "foo [color = \"0 0 255\", ]"))

(define (edge->string e)
  (match e
    ((edge from to attributes)
     (string-append (symbol->string from) " -> " (symbol->string to) " "
                    (attributes->attributes-list attributes)))))
(module+ test
  (check-equal? (edge->string (edge 'from 'to (hash)))
                "from -> to []")
  (check-equal? (edge->string (edge 'from 'to (hash 'weight 8)))
                "from -> to [weight = 8, ]")
  (check-equal? (edge->string (edge 'from 'to (hash 'weight 8 'color "255 255 0")))
                "from -> to [weight = 8, color = \"255 255 0\", ]"))

(define (outputify v)
  (cond [(string? v) (string-append "\"" v "\"")]
        [(number? v) (number->string v)]
        [(symbol? v) (symbol->string v)]))
(module+ test
  (check-equal? (outputify "foo") "\"foo\"")
  (check-equal? (outputify 3) "3")
  (check-equal? (outputify 'bar) "bar"))
