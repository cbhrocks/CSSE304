;Charles Horton

;1
(define fact
  (lambda (n)
    (if (zero? n)
      1
      (* n (fact (- n 1))))))

(define choose
  (lambda (n k)
    (/
      (fact n)
      (*
        (fact k)
        (fact (- n k))
        )
      )
    )
  )

;2
(define range
  (lambda (n m)
    (if
      (< n m)
      (cons 
        n 
        (range 
          (+ n 1) 
          m))
      '()
      )
    )
  )

;3
(define set?
  (lambda (n)
    (if
      (= 
        (length n)
        0
        )
      #t
      (if
        (member 
          (car n) 
          (cdr n))
        #f
        (set? 
          (cdr n)
          )
        )
      )
    )
  )

;4
(define sum-of-squares
  (lambda (n)
    (if 
      (= 
        (length n)
        0
        )
      0
      (+
        (* 
          (car n) 
          (car n)
          )
        (sum-of-squares (cdr n))
        )
      )
    )
  )
      
;5
(define make-vec-from-points
  (lambda (p1 p2)
    (cons
      (-
        (car p2)
        (car p1)
        )
      (cons
        (-
          (cadr p2)
          (cadr p1)
          )
        (cons
          (-
            (caddr p2)
            (caddr p1)
            )
          '()
          )
        )
      )
    )
  )

;6
(define dot-product
  (lambda (v1 v2)
    (+
      (+
        (*
          (car v1)
          (car v2)
          )
        (*
          (cadr v1)
          (cadr v2)
          )
        )
      (*
        (caddr v1)
        (caddr v2)
        )
      )
    )
  )

;7
(define vec-length
  (lambda (v)
    (sqrt 
      (sum-of-squares v)
      )
    )
  )

;8
(define distance
  (lambda (p1 p2)
    (sqrt
      (sum-of-squares
        (cons
          (-
            (car p2)
            (car p1)
            )
          (cons
            (-
              (cadr p2)
              (cadr p1)
              )
            (cons
              (-
                (caddr p2)
                (caddr p1)
                )
              '()
              )
            )
          )
        )
      )
    )
  )

;9
(define cross-product
  (lambda (v1 v2)
    (cons
      (-
        (*
          (caddr v2)
          (cadr v1)
          )
        (*
          (cadr v2)
          (caddr v1)
          )
        )
      (cons
        (-
          (*
            (car v2)
            (caddr v1)
            )
          (*
            (caddr v2)
            (car v1)
            )
          )
        (cons
          (-
            (*
              (cadr v2)
              (car v1)
              )
            (*
              (car v2)
              (cadr v1)
              )
            )
          '()
          )
        )
      )
    )
  )

;10
(define parallel?
  (lambda (v1 v2)
    (if
      (and
        (=
          (abs
            (/
              (car v2)
              (car v1)
              )
            )
          (abs
            (/
              (cadr v2)
              (cadr v1)
              )
            )
          )
        (=
          (abs
            (/
              (cadr v2)
              (cadr v1)
              )
            )
          (abs
            (/
              (caddr v2)
              (caddr v1)
              )
            )
          )
        )
      #t
      #f
      )
    )
  )

;11
(define collinear?
  (lambda (p1 p2 p3)
    (if
      (and
        (=
          (/
            (-
              (car p2)
              (car p1)
              )
            (-
              (cadr p2)
              (cadr p1)
              )
            )
          (/
            (-
              (car p3)
              (car p1)
              )
            (-
              (cadr p3)
              (cadr p1)
              )
            )
          )
        (=
          (/
            (-
              (cadr p2)
              (cadr p1)
              )
            (-
              (caddr p2)
              (caddr p1)
              )
            )
          (/
            (-
              (cadr p3)
              (cadr p1)
              )
            (-
              (caddr p3)
              (caddr p1)
              )
            )
          )
        )
      #t
      #f
      )
    )
  )

