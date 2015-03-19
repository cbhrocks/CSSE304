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

(define all-true?
  (lambda (n)
    (cond 
      [(null? n) #t]
      [(eq? (car n) #t)
       (all-true? (cdr n))]
      [else #f])
    )
  )

;1
(define multi-set?
  (lambda (n)
    (if (and (set? n)
             (all-true? (map list? n))
             (set? (map car n))
             (all-true? (map ms-pair? n)))
      #t
      #f)
    )
  )

(define ms-pair?
  (lambda (n)
    (if (and (list? n)
             (= (length n) 2)
             (symbol? (car n))
             (integer? (cadr n))
             (> (cadr n) 0))
      #t
      #f)
    )
  )

;2
(define ms-size
  (lambda (ms)
    (apply + (map cadr ms))
    )
  )

;3
(define matrix-ref
  (lambda (m row col)
      (list-ref (list-ref m row) col)
    )
  )

;4
(define matrix?
  (lambda (obj)
    (cond
      [(not (list? obj)) #f]
      [(null? obj) #f]
      [(not (list? (car obj))) #f]
      [(null? (car obj)) #f]
      [(not (same-lengths? obj (length (car obj))))
       #f]
      [else #t]
      )
    )
  )

(define same-lengths?
  (lambda (obj l)
    (cond 
      [(null? obj) #t]
      [(= (length (car obj)) l)
       (same-lengths? (cdr obj) l)]
      [else #f]
      )
    )
  )

;5
(define matrix-transpose
  (lambda (m)
    (list-of-cars m '())
    )
  )

(define list-of-cars
  (lambda (m n)
    (if
      (=(length (car m)) 0)
      n
      (list-of-cars (map cdr m) (append n (cons (map car m) '())))
      )
    )
  )

;6
(define last
  (lambda (ls)
    (if
      (= (length ls) 1)
      (car ls)
      (last (cdr ls))
      )
    )
  )
      

;7
(define all-but-last
  (lambda (l)
    (remove-last l '())
    )
  )

(define remove-last
  (lambda (l n)
    (if
      (= (length l) 1)
      n
      (remove-last (cdr l) (append n (cons (car l) '())))
      )
    )
  )
