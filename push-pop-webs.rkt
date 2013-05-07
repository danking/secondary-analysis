#lang racket

(provide (struct-out web)
         webset-from-relation)
(module+ test (require rackunit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions

;; A [Web A B] is a (web [Set A] [Set B])
(struct web (pushes pops) #:transparent)
(define empty-web (web (set) (set)))
;; A [Webset A B] is a (set [Web A B])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code

;; webset-from-relation : [Sequence [List A B]] -> [Webset A B]
(define (webset-from-relation R)
  (for/fold
      ((w (set)))
      ((pair R))
    (webset-add-connection w (first pair) (second pair))))

(module+ test
  (test-case
   "simple tests"
   (check-equal? (webset-from-relation (list ))
                 (set))
   (check-equal? (webset-from-relation (list (list 1 2)))
                 (set (web (set 1) (set 2))))
   (check-equal? (webset-from-relation (list (list 1 2)
                                             (list 3 2)))
                 (set (web (set 1 3) (set 2))))
   (check-equal? (webset-from-relation (list (list 1 2)
                                             (list 1 4)))
                 (set (web (set 1) (set 2 4))))
   (check-equal? (webset-from-relation (list (list 1 2)
                                             (list 1 4)
                                             (list 3 6)
                                             (list 5 6)))
                 (set (web (set 1) (set 2 4))
                      (web (set 3 5) (set 6)))))
  (test-case
   "merge tests"
   (check-equal? (webset-from-relation (list (list 1 2)
                                             (list 1 4)
                                             (list 3 6)
                                             (list 5 6)
                                             (list 1 6)))
                 (set (web (set 1 3 5) (set 2 4 6))))
   (check-equal? (webset-from-relation (list (list 1 2)
                                             (list 1 4)
                                             (list 3 6)
                                             (list 5 6)
                                             (list 5 4)))
                 (set (web (set 1 3 5) (set 2 4 6))))
   (check-equal? (webset-from-relation (list (list 1 2)
                                             (list 1 4)
                                             (list 3 6)
                                             (list 5 6)
                                             (list 3 2)))
                 (set (web (set 1 3 5) (set 2 4 6))))
   (check-equal? (webset-from-relation (list (list 1 2)
                                             (list 3 4)))
                 (set (web (set 1) (set 2))
                      (web (set 3) (set 4)))))
  (test-case
   "large test"
   (check-equal? (webset-from-relation (list (list 1 2)
                                             (list 3 4)
                                             (list 5 6)
                                             (list 1 8)
                                             (list 3 10)
                                             (list 3 12)
                                             (list 7 14)
                                             (list 9 14)
                                             (list 5 14)
                                             (list 3 14)
                                             (list 11 16)
                                             (list 13 18)
                                             (list 13 16)))
                 (set (web (set 1) (set 2 8))
                      (web (set 3 5 7 9) (set 6 4 10 12 14))
                      (web (set 11 13) (set 16 18))))))

;; webset-add-connection : [Webset A B] A B -> [Webset A B]
(define (webset-add-connection webs push pop)
  (let ((push-web (get-web/push webs push))
        (pop-web (get-web/pop webs pop)))
    (let ((new-webset (set-remove (set-remove webs push-web) pop-web)))
      (set-add new-webset (web-union push-web pop-web)))))

(module+ test
  (check-equal? (webset-add-connection (set)
                                       1 2)
                (set (web (set 1) (set 2))))
  (check-equal? (webset-add-connection (set (web (set 1) (set 2)))
                                       1 4)
                (set (web (set 1) (set 2 4))))
  (check-equal? (webset-add-connection (set (web (set 1) (set 2)))
                                       3 2)
                (set (web (set 1 3) (set 2))))
  (check-equal? (webset-add-connection (set (web (set 1) (set 2)))
                                       3 4)
                (set (web (set 1) (set 2))
                     (web (set 3) (set 4)))))

;; get-web/push : [Webset A B] A -> [Web A B]
(define (get-web/push webs push)
  (or (for/first ((web (in-set webs))
                  #:when (set-member? (web-pushes web) push))
        web)
      (web (set push) (set))))

(module+ test
  (check-equal? (get-web/push (set (web (set 1) (set 2))) 3)
                (web (set 3) (set)))
  (check-equal? (get-web/push (set (web (set 1) (set 2))) 2)
                (web (set 2) (set)))
  (check-equal? (get-web/push (set (web (set 1) (set 2))) 1)
                (web (set 1) (set 2)))
  (check-equal? (get-web/push (set (web (set 1) (set 2))
                                   (web (set 3) (set 4)))
                              1)
                (web (set 1) (set 2)))
  (check-equal? (get-web/push (set (web (set 1) (set 2))
                                   (web (set 3) (set 4)))
                              3)
                (web (set 3) (set 4))))

;; get-web/pop : [Webset A B] B -> [Web A B]
(define (get-web/pop webs pop)
  (or (for/first ((web (in-set webs))
                  #:when (set-member? (web-pops web) pop))
        web)
      (web (set) (set pop))))

(module+ test
  (check-equal? (get-web/pop (set (web (set 1) (set 2))) 3)
                (web (set) (set 3)))
  (check-equal? (get-web/pop (set (web (set 1) (set 2))) 1)
                (web (set) (set 1)))
  (check-equal? (get-web/pop (set (web (set 1) (set 2))) 2)
                (web (set 1) (set 2)))
  (check-equal? (get-web/pop (set (web (set 1) (set 2))
                                  (web (set 3) (set 4)))
                             2)
                (web (set 1) (set 2)))
  (check-equal? (get-web/pop (set (web (set 1) (set 2))
                                  (web (set 3) (set 4)))
                             4)
                (web (set 3) (set 4))))

;; web-union : [Web A B] [Web A B] -> [Web A B]
(define (web-union web1 web2)
  (web (set-union (web-pushes web1) (web-pushes web2))
       (set-union (web-pops web1) (web-pops web2))))

(module+ test
  (check-equal? (web-union (web (set) (set))
                           (web (set) (set)))
                (web (set) (set)))
  (check-equal? (web-union (web (set 1) (set 2))
                           (web (set) (set)))
                (web (set 1) (set 2)))
  (check-equal? (web-union (web (set) (set))
                           (web (set 1) (set 2)))
                (web (set 1) (set 2)))
  (check-equal? (web-union (web (set 1) (set 2))
                           (web (set 3) (set 4)))
                (web (set 1 3) (set 2 4))))
