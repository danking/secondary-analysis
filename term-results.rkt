#lang racket

(provide (struct-out term-results))

;; A Term-Results is a
;;   (term-results [Hash UID FlowValue]
;;                 [SetOf [List Term Term]]
;;                 [SetOf [List Term Term]]
;;                 [WebSet Term Term]
;;                 [Hash UID Term])
;;
(struct term-results
        (uid->fv/hash summaries callers push-pop-web uid->term/hash)
        #:transparent)

