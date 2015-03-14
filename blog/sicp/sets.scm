(declare (unit sets))

(define (element-of? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of? x set)
      set
      (cons x set)))

(define (intersection set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of? (car set1) set2)
         (cons (car set1)
               (intersection (cdr set1) set2)))
        (else (intersection (cdr set1) set2))))

;; Duplicate implemenation
(define (element-of? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of? x (cdr set)))))

(define (adjoin-set x set) (cons x set))

(define (intersection set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of? (car set1) set2)
         (cons (car set1)
               (intersection (cdr set1) set2)))
        (else (intersection (cdr set1) set2))))
