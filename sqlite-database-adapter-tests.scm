(use test)

(load "sqlite-database-adapter")

(test-group "Database (using Sqlite)"
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
