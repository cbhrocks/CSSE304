(define Fahrenheit->Celsius
  (lambda(n)
    (/ (- n 32) 
      9/5
      )
    )
  )

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

(define divisible-by-7?
  (lambda(n)
    (if 
      (=
        (modulo n 7)
        0)
      #t
      #f)
    )
  )

(define ends-with-7?
  (lambda(n)
    (if
      (= 
        (modulo 
           (- n 7)
           10)
        0)
      #t
      #f)
    )
  )

(define 1st
  (lambda(l)
    (car l)
    )
  )

(define 2nd
  (lambda(l)
    (cadr l)
    )
  )

(define 3rd
  (lambda(l)
    (caddr l)
    )
  )
