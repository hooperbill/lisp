(parse parse-form *parse* )

(define parse
 (lambda NIL
  (prog (f s )
    top (setq phrase NIL)
        (prin1 (quote form? ))
        (setq f (read ))
        (cond ((eq f (quote q ))
               (return (quote quit ))))
        (prin1 (quote sentence? ))
        (setq s (read ))
        (print (parse-form f s ))
        (print phrase )
        (go top ))))

(define parse-form 
 (lambda (form sentence )
  (cond ((null form )
         (null sentence ))
        ((null sentence)
         NIL)
        ((eq (car form )(quote * ))
         (cond ((null (cdr form))
                (setq *end NIL))
               (T
                (setq *end (cadr form))))
         (setq form (cdr form))
         (*parse* sentence))
        ((eq (car form )(car sentence ))
         (parse-form (cdr form )(cdr sentence ))))))

(define *parse*
 (lambda (sentence )
  (cond ((null sentence)
         (parse-form form sentence))
        ((eq (car sentence ) *end )
         (parse-form form sentence ))
        (T
         (setq phrase (append phrase (list (car sentence ))))
         (*parse* (cdr sentence ))))))
