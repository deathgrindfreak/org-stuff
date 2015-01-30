#lang racket

(require racket/pretty)
(require racket/format)

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (print-comparisons guess guess-factor f start-num)
  (let loop ((i 0) (guess guess) (x start-num))
    (when (< i 10)
      (let ((root (sqrt x))
            (iter (sqrt-iter guess x)))
        (printf "(sqrt ~a) => ~a, (sqrt-iter ~a ~a) => ~a diff => ~a~%"
                (~r x #:notation 'exponential)
                (~r root #:notation 'positional #:min-width 9)
                (~r guess #:notation 'exponential #:min-width 12)
                (~r x #:notation 'exponential)
                (~r iter #:notation 'positional #:min-width 9)
                (~r (- root iter) #:notation 'exponential #:min-width 9)))
      (loop (+ i 1) (f guess guess-factor) (/ x 10)))))

(print-comparisons 0.5 2 / 0.2)
