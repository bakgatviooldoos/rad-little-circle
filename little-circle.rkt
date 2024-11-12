#lang racket

(require plot
         math/number-theory)

; approximate the value of α according to a binary search
(define (approximate-α repeat)
  (define (F x)
    (+ (* 64 (expt x 6)) (* 64 (expt x 4)) (* 28 (expt x 2)) -3))
  
  (let loop ([lo         0.0]
             [hi         1.0]
             [last-error 0]
             [steps-left repeat])
    (define α     (/ (+ lo hi) 2)) ; binary search
    (define Δ     (* α (- (sqrt (+ (* 16 α α) 1)) 1)))
    (define r     (- (* 4 α α) (/ Δ (* 4 α))))
    (define error (F α)) ; we want the value at (F α) to be as close as possible to 0
    
    (cond [(zero? steps-left)
           (displayln
            (format "guesses have not converged after ~a steps" repeat))
           (values α Δ r error)]
          [(zero? error)
           (displayln
            (format "~a guesses made before converging on value" (- repeat steps-left)))
           (values α Δ r error)]
          [(equal? error last-error)
           (displayln
            (format "~a guesses made before converging on error" (- repeat steps-left 1)))
           (values α Δ r error)]
          [(positive? error)
           (loop lo α error (- steps-left 1))]
          [else
           (loop α hi error (- steps-left 1))])))

(define-values (α Δ r error)
  (approximate-α 60))

(define focus (vector 0 1/4))

(define (k x)
  (* (/ r (+ (* 2 α) Δ)) x))

(define (f x)
  (/ (- α x) (* 4 α)))

(define (g x)
  (* (- x α) (* 4 α)))

(define (h x)
  (+ (f (- x α)) (* 4 α α)))

(define (C α)
  (define Δ (* α (- (sqrt (+ (* 16 α α) 1)) 1)))
  (vector (+ (* 2 α) Δ) (- (* 4 α α) (/ Δ (* 4 α)))))

(define diagram-tree
  (list (x-axis)
        (y-axis)
        ; big circle
        (parametric
         (lambda (t) (vector (cos t) (sin t)))
         (- pi) pi
         #:color 1 #:label "x² + y² = 1")
        ; parabola
        (function
         sqr
         -1 1
         #:color 3 #:label "x²")
        ; parabola focus
        (point-label focus "focus" #:color 3)
        ; α
        (point-label (vector α 0) "α" #:color 2)
        ; focus to (α, 0)
        (function
         f
         -1 1
         #:color 3 #:style 'dot)
        ; parabola tangent through α
        (function
         g
         -1 1
         #:color 2 #:style 'dot)
        ; little circle/parabola norm
        (function
         h
         -1 1
         #:color 3 #:style 'dot)
        (point-label (vector (* 2 α) (* 4 α α)) "P" #:color 3)
        ; little circle/big circle norm
        (function k
                  -1 1
                  #:color 1 #:style 'dot)
        ; little circle
        (parametric
         (lambda (t)
           (vector (+ (* r (cos t)) (+ (* 2 α) Δ)) (+ (* r (sin t)) r)))
         (- pi) pi
         #:color 4)
        ; little circle center
        (point-label (vector (+ (* 2 α) Δ) r) "C" #:color 4)
        ; parameterization of the center of the little circle in terms of α
        (parametric C -1 1 #:color 4)))

(displayln
 (format "alpha:  ~a\nradius: ~a\nerror:  ~a" α r error))
(plot diagram-tree
      ;#:out-file "C:\\Users\\ChristiaanBrand\\OneDrive - Securicom\\backup\\racket\\nid\\little-circle.jpg"
      #:x-min -1.5 #:x-max 1.5
      #:y-min -1.5 #:y-max 1.5
      #:width  512 #:height 512)