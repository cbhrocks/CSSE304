;Charles Horton

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

(define set?
  (lambda (n)
    (cond
      [(not (list? n)) #f]
      [(=
         (length n)
         0
         )
       #t]
      [(member
         (car n)
         (cdr n))
       #f]
      [else
        (set?
          (cdr n)
          )]
      )
    )
  )

;1
(define nearest-point
  (lambda (p list-of-points)
    (find-nearest
      p
      (car list-of-points)
      (cdr list-of-points)
      )
    )
  )

(define find-nearest
  (lambda (p current-smallest list-of-points)
    (cond
      [(null? list-of-points) current-smallest]
      [(<
         (distance
           p
           (car list-of-points)
           )
         (distance
           p
           current-smallest
           )
         )
       (find-nearest
         p
         (car list-of-points)
         (cdr list-of-points)
         )]
      [else 
        (find-nearest p current-smallest
                      (cdr list-of-points)
                      )
        ]
      )
    )
  )

;2
(define union
  (lambda (s1 s2)
    (cond
      [(null? s2) s1]
      [(member 
         (car s2)
         s1)
       (union 
         s1 
         (cdr s2))]
      [else
        (union
          (cons (car s2) s1)
          (cdr s2))]
      )
    )
  )

;3
(define intersection
  (lambda (s1 s2)
    (create-intersection s1 s2 '())
    )
  )

(define create-intersection
  (lambda (s1 s2 s3)
    (cond
      [(null? s2) s3]
      [(member
         (car s2)
         s1)
       (create-intersection s1 (cdr s2) (cons (car s2) s3))]
      [else
        (create-intersection s1 (cdr s2) s3)
        ]
      )
    )
  )

;4
(define subset?
  (lambda (s1 s2)
    (cond
      [(null? s1) #t]
      [(member (car s1) s2) 
       (subset? (cdr s1) s2)]
      [else #f]
      )
    )
  )

;5
(define relation?
  (lambda (s1)
    (if
      (set? s1)
      (are-points? s1)
      #f
      )
    )
  )

(define are-points?
  (lambda (s1)
    (cond
      [(null? s1) #t]
      [(and
         (list? (car s1))
         (= (length (car s1)) 2))
       (are-points? (cdr s1))]
      [else #f]
      )
    )
  )

;6
(define domain
  (lambda (r)
    (create-domain r '())
    )
  )

(define create-domain
  (lambda (r d)
    (cond
      [(null? r)
       d]
      [(member (car (car r)) d)
       (create-domain (cdr r) d)]
      [else (create-domain (cdr r) (cons (car (car r))d))]
      )
    )
  )

;7
(define reflexive?
  (lambda (r)
    (check-reflexive r 0)
    )
  )

(define check-reflexive
  (lambda (r n)
    (if 
      (= n (length r))
      #t 
      (check-for-reflexives
        r
        n
        (car(list-ref r n))
        (cadr(list-ref r n)))
      )
    )
  )

(define check-for-reflexives
  (lambda (r n first second) 
    (cond
      [(not
         (member (cons first (cons first '())) r))
       #f]
      [(not
         (member (cons second (cons second '())) r))
       #f]
;      [(not
;         (member (cons second (cons first '())) r))
;       #f]
      [else
        (check-reflexive r (+ n 1))]
      )
    )
  )

(define hailstone-step-count
  (lambda (n)
    (if
      (= n 1)
      0
      (hailstone-helper n 0)
      )
    )
  )

(define hailstone-helper
  (lambda (n count)
    (cond
      [(= n 1)
       count]
      [(= (modulo n 2) 1)
      (hailstone-helper (+ (* n 3) 1) (+ count 1))]
      [else (hailstone-helper (/ n 2) (+ count 1))]
      )
    )
  )
