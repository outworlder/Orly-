(use test)
(use ssql)

(load "new-orly")
(load "database-adapter")
(load "dummy-database-adapter")
(load "sqlite-database-adapter-tests")

(define-class <test-model> (<model-base>)
  ((adapter (make <dummy-database-adapter>))
   (table 'test)
   (attributes '((a . 0) (b . 1)))))

(test-group "Base model"
            (test-assert "Creates a new model"
                         (not (null? (make <model-base>))))
            (test-group "Attribute setters and getters"
                        (test "Can get attribute data"
                              1
                              (get-attribute (make <model-base> 'attributes '((a . 1))) 'a))
                        (test-group "Set"
                                    (test "Returns the set value"
                                          2
                                          (set-attribute (make <model-base> 'attributes '((a . 1))) 'a 2))
                                    (let ((model (make <model-base> 'attributes '((z . 1)))))
                                      (test "Actually set the correct value"
                                            3
                                            (begin
                                              (set-attribute model 'z 3)
                                              (get-attribute model 'z))))
                                    (let ((model (make <model-base> 'attributes '((a .0) (b . 0) (c . 0)))))
                                      (test "Mass assignment works"
                                            '((a . 1) (b . 2) (c . 3))
                                            (begin
                                              (set-attributes model '((a 1) (b 2) (c 3)))
                                              (slot-value model 'attributes)))))))

;;; Note, multiple inserts: http://stackoverflow.com/questions/1609637/is-it-possible-to-insert-multiple-rows-at-a-time-in-an-sqlite-database/5009740#5009740
(test-group "Database calls"
            (let ((connection (db-connect (make <dummy-database-adapter>) "sadf")))
              (let ((model1 (make <test-model> 'connection connection))
                    (model2 (make <test-model> 'connection connection)))
                (test-group "Insert"
                            (test "Persistence flag - unsaved"
                                  #f
                                  (or (slot-value model1 'persisted) (slot-value model2 'persisted)))
                            (save model1)
                            (save model2)
                            (test "Generated SQL"
                                  (ssql->sql #f '(insert (into test) (columns a b) (values 0 1)))
                                  (slot-value connection 'last-statement))
                            (test "Number of inserted models"
                                  2
                                  (slot-value connection 'execute-count)))
                (test-group "Update"
                            (test "Persistence flag - saved"
                                  #t
                                  (and (slot-value model1 'persisted) (slot-value model2 'persisted)))))))
