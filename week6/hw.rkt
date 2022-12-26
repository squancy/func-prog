#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; #1
(define (sequence low high stride)
  (if (> low high)
    null
    (cons low (sequence (+ low stride) high stride))))

;; #2
(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))

;; #3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [else (car (list-tail xs (remainder n (length xs))))]))

;; #4
(define (stream-for-n-steps s n)
  (if (= n 0)
    null
    (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

;; #5
(define funny-number-stream
  (letrec ([f (lambda (x) (cons x (lambda () (cond [(< x 0) (f (* (- x 1) -1))]
                                                   [(= (modulo (+ x 1) 5) 0) (f (* (+ x 1) -1))]
                                                   [else (f (+ x 1))]))))])
    (lambda () (f 1))))

;; #6
(define dan-then-dog
  (letrec ([f (lambda (x) (cons x (lambda () (if (string=? x "dan.jpg")
                                                (f "dog.jpg")
                                                (f "dan.jpg")))))])
    (lambda () (f "dan.jpg"))))

;; #7
(define (stream-add-zero s)
  (letrec ([f (lambda (x) (cons (cons 0 (car x)) (lambda () (f ((cdr x))))))])
    (lambda () (f (s)))))

;; #8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (x)
                (cons (cons (list-nth-mod xs x) (list-nth-mod ys x))
                  (lambda () (f (+ x 1)))))])
  (lambda () (f 0))))

;; #9
(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
                (cond [(= (vector-length vec) n) #f]
                      [(not (pair? (vector-ref vec n))) (f (+ n 1))]
                      [(equal? v (car (vector-ref vec n))) (vector-ref vec n)]
                      [else (f (+ n 1))]))])
    (f 0)))

;; # 10
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [next-slot 0]
           [update-slot (lambda () (if (< next-slot n)
                                        (set! next-slot (+ 1 next-slot))
                                        (set! next-slot 0)))]
           [check-in-cache (lambda (v n) (cond [(equal? v (vector-ref cache n)) n]
                                               [(= (vector-length cache) (+ n 1)) #f]
                                               [else (check-in-cache v (+ n 1))]))]
           [f (lambda (v)
                (let ([cache-search (check-in-cache v 0)])
                  (if (not cache-search)
                    (let ([is-in-ls (assoc v xs)])
                      (if is-in-ls
                        (begin (vector-set! cache next-slot v) (update-slot) is-in-ls)
                        #f))
                    (vector-ref cache cache-search))))])
  (lambda (v) (f v))))

;; #11
(define-syntax while-less
  (syntax-rules (while-less do)
    [(while-less e1 do e2)
      (let ([e1-val e1])
        (letrec ([loop (lambda () (let ([e2-val e2])
                              (if (< e2-val e1-val)
                                  (loop)
                                  #t)))])
          (loop)))]))
