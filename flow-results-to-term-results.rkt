#lang racket

(require "term-results.rkt"
         "../pda-to-pda-risc/risc-enhanced/data.rkt"
         "push-pop-webs.rkt"
         "../semantics/flow.rkt"
         "../cfa2/lattice.rkt"
         "../cfa2/bp.rkt")

(provide flow-results->term-results
         uid->term
         uid->fv)

;; flow-results->term-results : Paths
;;                              Summaries
;;                              Callers
;;                              [Lattice FlowValue]
;;                              ->
;;                              Term-Results
(define (flow-results->term-results paths summaries callers lattice)
  (let ((term-summaries (flow-state-bpset->term-pair-set summaries))
        (flow-states (paths->flow-state-set paths)))

    (term-results (make-uid->flow-value/hash flow-states lattice)
                  term-summaries
                  (flow-state-bpset->term-pair-set callers)
                  (webset-from-relation term-summaries)
                  (make-uid->term flow-states))))

;; bpset->pair-set : [SetOf [BP A B]] -> [SetOf [List A B]]
(define (bpset->pair-set bpset)
  (for/set ((bp bpset))
    (BP->pair bp)))

;; flow-state-pair-set->term-pair-set : [SetOf [List FlowState FlowState]]
;;                                      ->
;;                                      [SetOf [List PDA-Term PDA-Term]]
(define (flow-state-pair-set->term-pair-set flow-state-pair-set)
  (for/set ((flow-state-pair (in-set flow-state-pair-set)))
    (map flow-state->node flow-state-pair)))

;; bpset->term-pair-set : [SetOf [BP FlowState FlowState]]
;;                        ->
;;                        [SetOf [List PDA-Term PDA-Term]]
(define (flow-state-bpset->term-pair-set bpset)
  (flow-state-pair-set->term-pair-set (bpset->pair-set bpset)))

;; flow-state->uid : FlowState -> UID
(define (flow-state->uid flow-state)
  (pda-term->uid (flow-state->node flow-state)))

;; paths->flow-state-set : Paths -> [SetOf FlowState]
;;
;; Produces a set of all the reachable flow states, given the paths set (which
;; defines reachability).
(define (paths->flow-state-set paths)
  (for/fold
      ((flow-state-set (set)))
      ((flow-state-pair (in-set (bpset->pair-set paths))))
    (set-add (set-add flow-state-set
                      (first flow-state-pair))
             (second flow-state-pair))))

;; make-uid->flow-value/hash : [SetOf FlowState]
;;                             [Lattice FlowValue]
;;                             ->
;;                             [Hash UID FlowValue]
(define (make-uid->flow-value/hash flow-states lattice)
  (for/fold
      ((fv-hash (hash)))
      ((flow-state (in-set flow-states)))
    (update-fv-hash fv-hash
                    lattice
                    (flow-state->uid flow-state)
                    (flow-state-flow-value flow-state))))

;; update-fv-hash : [Hash A FlowValue]
;;                  [Lattice FlowValue]
;;                  A
;;                  FlowValue
;;                  ->
;;                  [Hash A FlowValue]
(define (update-fv-hash fv-hash lattice index new-fv)
  (hash-set fv-hash
            index
            ((lattice-join lattice) (hash-ref fv-hash
                                              index
                                              (lattice-top lattice))
                                    new-fv)))

;; make-uid->term : [SetOf FlowState] -> [Hash UID PDA-Term]
(define (make-uid->term flow-states)
  (for/hash ((flow-state (in-set flow-states)))
    (let ((pda-term (flow-state->node flow-state)))
      (values (pda-term->uid pda-term) pda-term))))

;; uid->term : Term-Results UID -> Term
(define (uid->term term-results uid)
  (hash-ref (term-results-uid->fv/hash term-results) uid))

;; uid->fv : Term-Results UID -> FlowValue
(define (uid->fv term-results uid)
  (hash-ref (term-results-uid->term/hash term-results) uid))
