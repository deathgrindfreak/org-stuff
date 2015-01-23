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
