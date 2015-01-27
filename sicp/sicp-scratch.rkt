;;; Ex2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split (- n 1))))
        (below painter (beside smaller smaller)))))


;;; Ex2.45
(define (split left-op right-op)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller (split (- n 1))))
          (left-op painter (right-op smaller smaller))))))


(/ (+ 5 4 (- 2 3 (+ 6 (/ 4 5))))
   (* 3 (- 6 2) (- 2 7)))

(define (square-sum-of-max-of-two a b c)
  (cond ((or (> a b c)
             (> b a c)) (+ (* a a)
                           (* b b)))
        ((or (> a c b)
             (> c a b)) (+ (* a a)
                           (* c c)))
        ((or (> b c a)
             (> c b a)) (+ (* b b)
                           (* c c)))))

(test 0 (p)) => (if (= x 0) => 0
                    0
                    (p))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause else-clause)
        (else else-clause)))

(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))
