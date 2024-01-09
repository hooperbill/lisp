(guess )

(define guess 
  (lambda NIL
    (prog (n g c )
      (setq n (mod (random 0 )100 ))
      (setq c 1 )
      top
      (prin1 (quote (guess a number from 0 to 100 ? )))
      (setq g (read ))
      (cond 
        ((eq n g )
         (return (append (quote (you guessed correctly in ))(ncons c ))))
        ((greaterp g n )
         (print (quote (to high ))))
        ((lessp g n )
         (print (quote (to low )))))
     (setq c (add1 c ))
     (go top ))))
NIL 


