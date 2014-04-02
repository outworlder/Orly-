(use miscmacros)

(define-class <dummy-connection> (<connection>)
  ((execute-count 0)
   (fetch-count 0)
   (last-statement #f)))

(define-class <dummy-database-adapter> (<database-adapter>))

(define-generic (db-connect <dummy-database-adapter> <string>))
(define-method (db-connect (adapter <dummy-database-adapter>) (connection-string <string>))
  (make <dummy-connection>))

(define-method (db-execute (connection <dummy-connection>) (raw-sql <string>))
  (inc! (slot-value connection 'execute-count))
  (set! (slot-value connection 'last-statement) raw-sql)
  1)

(define-method (db-fetch (connection <dummy-connection>) (raw-sql <string>))
  (inc! (slot-value connection 'fetch-count))
  (set! (slot-value connection 'last-statement) raw-sql))

(define-method (reset-counters! (connection <dummy-connection>))
  (set! (slot-value connection 'fetch-count) 0)
  (set! (slot-value connection 'execute-count) 0)
  (set! (slot-value connection 'last-statement) #f))
