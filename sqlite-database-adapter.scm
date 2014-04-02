(use sql-de-lite)

(define-class <sqlite-connection> (<connection>)
  ((connection-info #f)))

(define-class <sqlite-database-adapter> (<database-adapter>))

(define-method (db-connect (adapter <sqlite-database-adapter>) (connection-string <string>))
  (let ((result (open-database connection-string)))
    (if result
        (make <sqlite-connection> 'open #t 'connection-info result)
        #f)))

(define-method (db-execute (connection <sqlite-connection>) (raw-statement <string>))
  (exec (sql (slot-value connection 'connection-info) raw-statement)))

(define-method (db-fetch (connection <sqlite-connection>) (raw-statement <string>))
  (query fetch-all (sql (slot-value connection 'connection-info) raw-statement)))
