(define interval-contains?
  (lambda(n m)
    (and
      (<
        (-
          (list-ref n 0)
          m)
        1)
      (>
        (-
          (list-ref n 1)
          m)
        -1)
      )
    )
  )

(define interval-intersects?
  (lambda(i1 i2)
    (or
      (or
        (interval-contains? i1 (list-ref i2 0))
        (interval-contains? i1 (list-ref i2 1))
        )
      (or
        (interval-contains? i2 (list-ref i1 0))
        (interval-contains? i2 (list-ref i1 0))
        )
      )
    )
  )


(define interval-union
  (lambda(i1 i2)
    (if (interval-intersects? i1 i2)
      (cons
        (cons
          (if
            (<
              (list-ref i1 0)
              (list-ref i2 0)
              )
            (list-ref i1 0)
            (list-ref i2 0)
            )
          (cons
            (if
              (>
                (list-ref i1 1)
                (list-ref i2 1)
                )
              (list-ref i1 1)
              (list-ref i2 1)
              )
            '()
            )
          )
        '()
        )
      (cons i1 (cons i2 '()))
      )
    )
  )

;1
(define minimize-interval-list
  (lambda (ls)
    (minimize-interval-list-helper ls 1)
    )
  )

(define minimize-interval-list-helper
  (lambda (ls n)
    (cond
      [(null? ls)
       '()]
      [(= n (length ls))
       (cons (car ls) (minimize-interval-list-helper (cdr ls) 1))]
      [(interval-intersects? (car ls) (list-ref ls n))
       (minimize-interval-list-helper 
         (append (interval-union (car ls) (list-ref ls n)) 
               (cdr (remove-n-from-list ls n))) n)]
      [else (minimize-interval-list-helper ls (+ n 1))]
      )
    )
  )


;2
;if #t is passed in for spot, then it returns the distance from the end of the list that the predicate apears otherwise returns #t
(define if-one-true
  (lambda (l spot?)
    (cond
      [(= (length l) 0) #f]
      [(eq? (car l) #t) 
       (if 
         (eq? spot? #t)
         (length l)
         #t)]
      [else (if-one-true (cdr l) spot?)]
      )
    )
  )

(define exists?
  (lambda (pred ls)
    (if-one-true (map pred ls) #f)
    )
  )

;3
(define list-index
  (lambda (pred ls)
    (if
      (exists? pred ls)
      (- (length ls) (if-one-true (map pred ls) #t))
      #f
      )
    )
  )

;4
(define pascal-triangle
  (lambda (n)
    (if
      (< n 0)
      '()
      (pascal-helper n '((1)))
      )
    )
  )

(define pascal-helper
  (lambda (n l)
    (if
      (= n 0) 
      l
      (pascal-helper (- n 1) (cons (pascal-list-helper (car l) '()) l))
      )
    )
  )

(define pascal-list-helper
  (lambda (l1 l2)
    (cond
      [(null? l2)
       (pascal-list-helper l1 (append l2 (cons (car l1) '())))]
      [(> (length l1) 1)
       (pascal-list-helper (cdr l1) (append l2 (cons (+ (car l1) (cadr l1)) '())))]
      [else (append l2 l1)]
      )
    )
  )

;5
(define product
  (lambda (s1 s2)
    (if
      (null? s1)
      '()
      (append (distribute (car s1) s2) (product (cdr s1) s2))
      )
    )
  )

(define distribute
  (lambda (a s1)
    (if
      (null? s1)
      '()
      (cons (cons a (cons (car s1) '())) (distribute a (cdr s1)))
      )
    )
  )

;6
(define max-edges
  (lambda (n)
    (/ (* n (- n 1)) 2)
    )
  )

;7
(define complete?
  (lambda (G)
    (complete?-helper G (- (length G) 1))
    )
  )

(define complete?-helper
  (lambda (G n)
    (cond
      [(< n 0)
       #t]
      [(in-all-but-n (car (list-ref G  n)) (map cdr G) n (- (length G) 1))
       (complete?-helper G (- n 1))]
      [else
        #f]
      )
    )
  )

(define in-all-but-n
  (lambda (s l n c)
    (cond
      [(< c 0)
       #t]
      [(= c n)
       (in-all-but-n s l n (- c 1))]
      [(member s (car (list-ref l c)))
       (in-all-but-n s l n (- c 1))]
      [else #f]
      )
    )
  )

;8
(define complete
  (lambda (ls)
    (complete-helper ls (- (length ls) 1))
    )
  )

(define complete-helper
  (lambda (ls n)
    (if
      (< n 0)
      '()
      (cons (cons (list-ref ls n) (cons (remove-n-from-list ls n) '())) (complete-helper ls (- n 1)))
      )
    )
  )

(define remove-n-from-list
  (lambda (ls n)
    (cond
      [(null? ls)
       '()]
      [(= n 0)
       (remove-n-from-list (cdr ls) (- n 1))]
      [else (cons (car ls) (remove-n-from-list (cdr ls) (- n 1)))]
      )
    )
  )

;9
(define replace
  (lambda (old new ls)
    (cond
      [(null? ls)
       '()]
      [(eq? (car ls) old)
       (cons new (replace old new (cdr ls)))]
      [else (cons (car ls) (replace old new (cdr ls)))]
      )
    )
  )

;10
(define remove-first
  (lambda (element ls)
    (cond
      [(null? ls)
       ls]
      [(eq? (car ls) element)
       (cdr ls)]
      [else (cons (car ls) (remove-first element (cdr ls)))]
      )
    )
  )

;11
(define remove-last
  (lambda (element ls)
    (cond
      [(null? ls)
       ls]
      [(and
         (eq? (car ls) element)
         (not (member element (cdr ls))))
       (cdr ls)]
      [else (cons (car ls) (remove-last element (cdr ls)))]
      )
    )
  )
