
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; test
(define ones (lambda () (cons 1 ones)))

;; put your code below
(define (sequence low high stride)
  (if (> low high) null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda(x) (string-append x suffix))
       xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error 'list-nth-mod "negative number")]
        [(null? xs) (error 'list-nth-mod "empty list")]
        [#t (begin (define i (remainder n (length xs)))
                   (car (list-tail xs i)))]))

(define (stream-for-n-steps s n)
  (if (< n 1) null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

(define funny-number-stream
  (letrec ([f (lambda (x)
                (cons (if (= (remainder x 5) 0) (- x) x) (lambda () (f (+ x 1)))))])
    (lambda() (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (x flag)
                (cons (if flag "dan.jpg" "dog.jpg") (lambda () (f x (not flag)))))])
    (lambda() (f "dan.jpg" #t))))
                
(define (stream-add-zero s)
  (letrec ([f (lambda()
                (cons (cons 0 (car (s))) (lambda() ((stream-add-zero (cdr (s)))))))])
    f))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda() (f (+ n 1)))))])
    (lambda() (f 0))))

(define (vector-assoc x vec)
  (letrec ([len (vector-length vec)]
            [f (lambda (loc)
                 (cond [(> loc (- len 1)) #f]
                       [(pair? (vector-ref vec loc)) (if (equal? (car (vector-ref vec loc)) x)
                                                         (vector-ref vec loc) (f (+ loc 1)))]
                       [#t (f (+ loc 1))]))])
    (f 0)))

(define (cached-assoc xs n)
  (letrec([cache (make-vector n #f)]
          [loc 0]
          [f (lambda (v)
               (cond [(pair? (vector-assoc v cache)) (vector-assoc v cache)]
                     [(pair? (assoc v xs)) (begin (define lst-res (assoc v xs))
                                                  (vector-set! cache (remainder loc n) lst-res)
                                                  (set! loc (+ loc 1))
                                                  lst-res)]
                     [#t #f]))])
    f))
                     
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (let ([v1 e1])
       (letrec ([loop (lambda(it)
                        (if (> it (- v1 1))
                            #t
                            (begin (set! it e2) (loop it))))])
         (loop e2)))]))