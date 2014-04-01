(use coops)
(use coops-primitive-objects)

(define-class <database-adapter> ())
(define-class <connection> ()
  ((open #f)))

(define-generic (db-connect <database-adapter> <string>))
(define-generic (db-execute <connection> <string>))
(define-generic (db-fetch <connection> <string>))
