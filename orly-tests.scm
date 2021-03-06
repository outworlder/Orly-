(use test)

(use sql-de-lite)
(use aql)
(use coops)
(use orly)

(orly-database-url "orly-test.db")
(orly-debug #t)

(if (file-exists? (orly-database-url))
    (delete-file (orly-database-url)))

(define database-fixtures
  '("CREATE TABLE test1 (
                    id INTEGER,
                    col2 TEXT,
                    col3 TEXT
                    );"
    "CREATE TABLE test_parent (
    id INTEGER,
    col2 TEXT,
    col3 TEXT
   );"
    "CREATE TABLE test_child (
                         id INTEGER,
                         parent_id TEXT,
                         col3 TEXT
                         );"

    "insert into test1 (id, col2, col3) values (1, \"col2\", \"col3\");"
    "insert into test1 (id, col2, col3) values (2,\"col22\", \"col23\");"))

(define-model <test1> "test1"
  (id col2 col3))

(define-model <test-parent> "test_parent"
  (id col2 col3))

(has-many <test-parent> <test-child> children foreign-key: parent_id)

(define-model <test-child> "test_child"
  (id parent_id col3))

(belongs-to <test-child> <test-parent> parent foreign-key: parent_id)

(for-each (lambda (stmt)
            (call-with-database (orly-database-url)
                                (lambda (database)
                                  (exec (sql database stmt))))) database-fixtures)

;;; TODO: Execute SQL here.

(test-group "Queries"
            (test "Should be able to query a record by id" (make <test1> 'id 1 'col2 "col2" 'col3 "col3") (find-by-id <test1> 1))
            (test "Should return an empty list if a record is not found" '() (find-by-id <test1> 99999))
            (test "Should return a list of all rows in the database" 2 (length (find-all <test1>)))
            (test "Should be able to find by ID and a condition" (make <test1> 'id 1 'col2 "col2" 'col3 "col3") (find-by-id <test1> 1 conditions: `(where (= 'col2 "'col2'"))))
            (test "Should be able to find all records with a condition"
                  (list (make <test1> 'id 1 'col2 "col2" 'col3 "col3"))
                  (find-all <test1> conditions: `(where (= 'col2 "col2")))))

;(test-group "Relationships")

(delete-file (orly-database-url))