(use test)

(load "new-orly")
(load "sqlite-database-adapter")

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
(test-group "Database tests (using Sqlite)"
            (let ((connection (db-connect (make <sqlite-database-adapter>) "test.sqlite")))
              (test-group "'Execute' statements"
                          (test "Dropping any existing tables"
                                0
                                (db-execute connection "drop table if exists test;"))
                          (test "Creating a table"
                                0
                                (db-execute connection "create table test(x integer primary key asc, y);"))
                          (test "Test data insertion"
                                3
                                (db-execute connection "insert into 'test' select null as x, 't1' as 'y' union select null, 't2' union select null, 't3';")))
              (test-group "Fetch statemetnts"
                          (test "Count the number of items in the table"
                                3
                                (car (db-execute connection "select count(*) from test;")))
                          (test "Retrieve the items"
                                '((1 "t1") (2 "t2") (3 "t3"))
                                (db-fetch connection "select * from test;")))))
