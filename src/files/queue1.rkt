#lang racket
 
(define (make-queue) (mcons #f #f))
(define (push! q x)
  (define new (mcons x #f))
  (if (mcar q) (set-mcdr! (mcdr q) new) (set-mcar! q new))
  (set-mcdr! q new))
(define (pop! q)
  (define old (mcar q))
  (cond [(eq? old (mcdr q)) (set-mcar! q #f) (set-mcdr! q #f)]
        [else (set-mcar! q (mcdr old))])
  (mcar old))
(define (empty? q)
  (not (mcar q)))
 
(define Q (make-queue))
(empty? Q) ; -> #t
(push! Q 'x)
(empty? Q) ; -> #f
(for ([x 3]) (push! Q x))
(pop! Q)   ; -> 'x
(list (pop! Q) (pop! Q) (pop! Q)) ; -> '(0 1 2)

;; Invariants:
;; The elements in the queue are (append front (reverse back)).
;; Front is always non-empty (except for the empty queue).
(struct queue (front back))
 
(define empty (queue '() '()))
 
(define (push x q)
  (if (null? (queue-front q))
      (queue (reverse (cons x (queue-back q))) '())
      (queue (queue-front q) (cons x (queue-back q)))))

(define (empty? q)
  (null? (queue-front q)))
 
(define (pop q)
  (cond [(empty? q) (error 'pop "the queue is empty")]
        [(not (null? (queue-front q)))
         (if (null? (rest (queue-front q)))
             (queue (reverse (queue-back q)) '())
             (queue (rest (queue-front q)) (queue-back q)))]
        [else (queue (reverse (queue-back q)) '())]))
 
(define (first q)
  (cond [(empty? q) (error 'first "the queue is empty")]
        [(car (queue-front q))]))
 
;; Example:
(first (pop (pop (for/fold ([q empty]) ([x '(1 2 3 4)])
                   (push x q)))))
;; => 3