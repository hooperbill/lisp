(sum )
(define sum
  (lambda x 
    (do 
      ((t 0 (plus t (arg c )))
       (c x (sub1 c ))
      )
      ((zerop c )t )
    )
  )
)
NIL 
