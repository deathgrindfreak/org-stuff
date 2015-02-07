#lang racket

(require racket/draw)


(define *width* 300)
(define *height* 300)
(define *target* (make-bitmap *width* *height*))
(define *dc* (new bitmap-dc% [bitmap *target*]))

(send *dc* draw-rectangle 0 0 *width* *height*)
(send *dc* set-pen "black" 2 'solid)


(define (make-vect x y)
  (list x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cadr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1)
                (xcor-vect v2))
             (- (ycor-vect v1)
                (ycor-vect v2))))

(define (scale-vect m v2)
  (make-vect (* m (xcor-vect v2))
             (* m (ycor-vect v2))))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(define (make-segment v1 v2)
  (list v1 v2))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cadr segment))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))


(define (draw-line start end)
  (send *dc* draw-line
        (* *width* (xcor-vect start))
        (* *height* (ycor-vect start))
        (* *width* (xcor-vect end))
        (* *height* (ycor-vect end))))


(define (paint-outline frame)
  (let ((origin (make-vect 0.0 0.0))
        (edge1 (make-vect 1.0 0.0))
        (edge2 (make-vect 0.0 1.0))
        (opposite-origin (make-vect 1.0 1.0)))
    ((segments->painter (list (make-segment origin edge1)
                              (make-segment origin edge2)
                              (make-segment edge1 opposite-origin)
                              (make-segment edge2 opposite-origin)))
     frame)))

(define (paint-x frame)
  (let ((origin (make-vect 0.0 0.0))
        (edge1 (make-vect 1.0 0.0))
        (edge2 (make-vect 0.0 1.0))
        (opposite-origin (make-vect 1.0 1.0)))
    ((segments->painter (list (make-segment origin opposite-origin)
                              (make-segment edge1 edge2)))
     frame)))

(define (paint-diamond frame)
  (let ((origin-edge1 (make-vect 0.0 0.5))
        (origin-edge2 (make-vect 0.5 0.0))
        (edge1-opp (make-vect 0.5 1.0))
        (edge2-opp (make-vect 1.0 0.5)))
    ((segments->painter (list (make-segment origin-edge1 origin-edge2)
                              (make-segment origin-edge1 edge1-opp)
                              (make-segment origin-edge2 edge2-opp)
                              (make-segment edge1-opp edge2-opp)))
     frame)))


(define (paint-wave frame)
  ((segments->painter
    (transform-ys (list
                        
                   ;; Head
                   (make-segment (make-vect 0.38 0.87)
                                 (make-vect 0.41 1.00))
                   (make-segment (make-vect 0.38 0.87)
                                 (make-vect 0.41 0.76))
                   (make-segment (make-vect 0.61 0.87)
                                 (make-vect 0.58 1.00))
                   (make-segment (make-vect 0.61 0.87)
                                 (make-vect 0.58 0.76))
                   
                   ;; Shoulders
                   (make-segment (make-vect 0.41 0.76)
                                 (make-vect 0.34 0.76))
                   (make-segment (make-vect 0.58 0.76)
                                 (make-vect 0.72 0.76))

                   ;; Arms
                   (make-segment (make-vect 0.72 0.76)
                                 (make-vect 1.00 0.48))
                   (make-segment (make-vect 0.58 0.58)
                                 (make-vect 1.00 0.33))
                   (make-segment (make-vect 0.36 0.60)
                                 (make-vect 0.34 0.68))
                   (make-segment (make-vect 0.34 0.68)
                                 (make-vect 0.16 0.50))
                   (make-segment (make-vect 0.16 0.50)
                                 (make-vect 0.00 0.72))
                   (make-segment (make-vect 0.16 0.68)
                                 (make-vect 0.00 0.88))
                   (make-segment (make-vect 0.16 0.68)
                                 (make-vect 0.34 0.76))

                   ;; Legs
                   (make-segment (make-vect 0.58 0.58)
                                 (make-vect 0.75 0.00))
                   (make-segment (make-vect 0.50 0.42)
                                 (make-vect 0.60 0.00))
                   (make-segment (make-vect 0.50 0.42)
                                 (make-vect 0.40 0.00))
                   (make-segment (make-vect 0.36 0.60)
                                 (make-vect 0.25 0.00))
                   
                   )))
   frame))

(define (transform-ys lst)
  (map (lambda (seg)
         (let ((st (start-segment seg))
               (ed (end-segment seg)))
           (make-segment (make-vect (xcor-vect st)
                                    (- 1 (ycor-vect st)))
                         (make-vect (xcor-vect ed)
                                    (- 1 (ycor-vect ed))))))
       lst))

(paint-wave (make-frame (make-vect 0.0 0.0)
                        (make-vect 1.0 0.0)
                        (make-vect 0.0 1.0)))

(send *target* save-file "paint-wave.png" 'png)

;; (send *dc* draw-line
;;       0 0
;;       30 30)
