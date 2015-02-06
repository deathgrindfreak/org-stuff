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


(define (paint-outline frame)
  (let ((origin (0.0 0.0))
        (edge1 (1.0 0.0))
        (edge2 (0.0 1.0))
        (opposite-origin (1.0 1.0)))
    ((segments->painter (list (make-segment origin edge1)
                              (make-segment origin edge2)
                              (make-segment edge1 opposite-origin)
                              (make-segment edge2 opposite-origin)))
     frame)))

(define (paint-x frame)
  (let ((origin (0.0 0.0))
        (edge1 (1.0 0.0))
        (edge2 (0.0 1.0))
        (opposite-origin (1.0 1.0)))
    ((segments->painter (list (make-segment origin opposite-origin)
                              (make-segment edge1 edge2)))
     frame)))


(define (paint-diamond frame)
  (let ((origin-edge1 (0.0 0.5))
        (origin-edge2 (0.5 0.0))
        (edge1-opp (0.5 1.0))
        (edge2-opp (1.0 0.5)))
    ((segments->painter (list (make-segment origin-edge1 origin-edge2)
                              (make-segment origin-edge1 edge1-opp)
                              (make-segment origin-edge2 edge2-opp)
                              (make-segment edge1-opp edge2-opp)))
     frame)))

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate-180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate-270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((painter-below (transform-painter painter 1
                                            (make-vect 0.0 0.0)
                                            (make-vect 1.0 0.0)
                                            split-point))
          (painter-above (transform-painter painter2
                                            split-point
                                            (make-vect 1.0 0.5)
                                            (make-vect 0.0 1.0))))
      (lambda (frame)
        (painter-below frame)
        (painter-above frame)))))

(define (below painter1 painter2)
  (rotate-270 (beside painter1 painter2)))
