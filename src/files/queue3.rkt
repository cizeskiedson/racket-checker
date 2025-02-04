
(define (heap)
  (let ((the-heap (make-vector 10)) (next-free 1))

(define (swim val) 
  (begin 
    (vector-set! the-heap next-free val)
    (set! next-free (+ next-free 1))
    (let loop ((i (- next-free 1)))
      (if (and (< (vector-ref the-heap (quotient i 2)) (vector-ref the-heap i)) (> (quotient i 2) 0))
          (begin 
            (exch the-heap i (floor (/ i 2)))
            (loop (floor (/ i 2))))
        the-heap))))

(define (sink index) 
  ;; a call to this method is used to re-establish order within the
  ;; heap by 'sinking' the value rooted at index to its right position
  ;; within the whole heap
  (let loop ((node index) (kid (* 2 index)))
    (if (<= kid next-free)
        (let ((larger-kid (if (> (vector-ref the-heap kid) (vector-ref the-heap (+ kid 1)))
                                 kid
                                 (+ kid 1))))
          (if (> (vector-ref the-heap larger-kid) (vector-ref the-heap node))
              (begin
                (exch the-heap node larger-kid)
                (loop larger-kid (* 2 larger-kid)))
              the-heap))
          (newline))))

(define (delete-max)
  ;; delete and return the maximum value in the heap.
  ;; add the last item on the heap to the root position 
  ;; sink to re-establish order within the heap of the heap and call
  ;; 
  (begin 
    (let ((val (vector-ref the-heap 1)))
      (vector-set! the-heap 1 (vector-ref the-heap (- next-free 1)))
      (set! next-free (- next-free 1))
      (vector-set! the-heap next-free 0)
      (sink 1)
      val)))


(define (put value) 
  (swim value))

(define (size) 
  (vector-length the-heap))

(define (empty?) 
  (= next-free 1))

(define (exch vec lo j)
  (let ((lo-val (vector-ref vec lo)) (j-val (vector-ref vec j)))
    (begin
      (vector-set! vec lo j-val)
      (vector-set! vec j lo-val))))

(define (dispatch message . arg)
  (cond 
    ((eq? message 'delete-max) (delete-max))
    ((eq? message 'put) (put (car arg)))
    ((eq? message 'size) size)
    ((eq? message 'heap) the-heap)
    ((eq? message 'empty?) empty?)))
dispatch))