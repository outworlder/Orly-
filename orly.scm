(module orly
  (orly-database-url
   orly-debug
   <table>
   <model>
   define-model
   has-many
   belongs-to
   find-all
   find-first
   find-by-id
   find-all-by-id
   find-all-by-string
   find-first-by-string
   insert-model
   update-model)

  (import chicken scheme data-structures srfi-1 coops sql-de-lite)

  (define orly-database-url
    (make-parameter #f))

  (define orly-debug
    (make-parameter #f))
  
  (define-class <table> ()
    (name columns (relationships initform: '())))

  (define-class <model> ()
    (table))

  (define-syntax with-model
    (syntax-rules ()
      ([_ class function]
       (let* ([klass (make class)]
              [table-name (get-table-name klass)]
              [table-columns (list-columns klass)])
         (function class table-name table-columns)))))

  (define-syntax car-if-not-empty
    (syntax-rules ()
      ([_ expression]
       (let ([result expression])
         (if (null? result)
             result
             (if (list? result)
                 (car result)
                 result))))))

  ;; Simplification: the first column will be the ID
  (define-syntax define-model
    (syntax-rules ()
      ([_ model-name database-table-name (table-columns ...)]
       (define-class model-name (<model>)
         (table-columns ... (table initform: (make <table> 'name 'database-table-name 'columns '(table-columns ...))) )) )))

  (define-syntax has-many
    (syntax-rules (foreign-key: primary-key:)
      ([_ parent child method-name foreign-key: column-name primary-key: key-name]
       (define-method (method-name (model parent))
         (find-all-by-id child (slot-value model (quote column-name)) primary-key: (quote key-name))))
      ([_ parent child method-name foreign-key: column-name]
       (define-method (method-name (model parent))
         (find-all-by-id child (slot-value model (quote column-name)) primary-key: (quote column-name))))))

                                        ;(has-many <planet-schematics> <planet-schematics-typemap> typemaps foreign-key: schematicID)

  (define-syntax belongs-to
    (syntax-rules (foreign-key: primary-key:)
      ([_ child parent method-name foreign-key: column-name primary-key: key-name]
       (define-method (method-name (model child))
         (find-by-id parent (slot-value model (quote column-name)) primary-key: (quote key-name))))
      ([_ child parent method-name foreign-key: column-name]
       (define-method (method-name (model child))
         (find-by-id parent (slot-value model (quote column-name) ))))))

  ;; (define-syntax has-and-belongs-to-many
  ;;   (syntax-rules (foreign-keys:)
  ;;     ([_ parent child method-name foreign-key: (column-names)]
  ;;      (define-method (method-name (model parent))
  ;;        (find-all-by-id child (slot-value model (quote column-name)))))))

  ;; (has-and-belongs-to-many <planet-schematics> <planet-schematics-typemap> <inventory-types> foreign-keys: (schematicID typeID))

                                        ;(belongs-to <planet-schematics-typemap> <planet-schematics> schematic foreign-key: schematicID)
  (define-method (get-table-class (model <model>))
    (slot-value model 'table))

  (define-method (get-table-name (model <model>))
    (slot-value (get-table-class model) 'name))

  (define-method (get-table-id (model <model>))
    (car (list-columns model)))

  (define-method (list-columns (model <model>))
    (slot-value (get-table-class model) 'columns))

  (define-method (insert-model (instance <model>))
    (with-model (class-of instance)
                (lambda (instance table-name table-columns)
                  (execute-sql `(insert ,table-name ,table-columns ,(map (lambda (i) '?) table-columns)))
                  (let ([inserted-id 
                         (car (execute-sql "select last_insert_rowid() as last_insert_rowid"))])
                    (set! (slot-value instance (get-table-id)) inserted-id)
                    instance))))

  (define-method (update-model (instance <model>))
    (with-model (class-of instance)
                (lambda (instance table-name table-columns)
                  (let ([values (map (lambda (column)
                                       (slot-value instance column)))])
                    (execute-sql `(update ,table-name ,(zip table-columns values)))))))
  
  (define (find-all class #!key conditions)
    (with-model class
                (lambda (class table-name table-columns)
                  (load-data class (execute-sql (eval (make-select table-columns table-name conditions: conditions)))))))

  (define (find-first class #!key conditions)
    (with-model class
                (lambda (class table-name table-columns)
                  (car-if-not-empty (load-data class (execute-sql (eval (append (make-select table-columns table-name conditions: conditions) '((limit 1))))))))))

                                        ; Optimize this! It should ask the database for just one row
  (define (find-by-id class id #!key primary-key)
    (car-if-not-empty (find-all-by-id class id primary-key: primary-key)))

  (define (find-all-by-id class id #!key primary-key)
    (with-model class
                (lambda (class table-name table-columns)
                  (let ([key (if primary-key
                                 primary-key
                                 (car table-columns))])
                    (load-data class (execute-sql (eval (append (make-select table-columns table-name conditions: `(where (= (quote ,key) ,id)))))))))))

  (define (find-all-by-string class key name)
    (find-all class conditions: `(where (like (quote ,key) ,(string-append "%" name "%")))))

  (define (find-first-by-string class key name)
    (find-all class conditions: `(where (like (quote ,key) ,(string-append "%" name "%")))))

  (define (make-select columns table #!key conditions id limit)
    (let ([limit-stmt (if limit
                          `(limit ,limit)
                          #f)])
      `(from ,table (,@columns) ,conditions ,limit-stmt)))

  (define (make-where-id columns id)
    `(where (= ,(car columns) ,id)))

  (define-method (save (model <model>))
    #f)

  (define (execute-sql stmt)
    (call-with-database (orly-database-url)
                        (lambda (database)
                          (if (orly-debug)
                              (print "Executing: " stmt))
                          (query fetch-all (sql database stmt)))))

  (define (load-data class data)
    (if (list? data)
        (if (null? data)
            data
            (map (lambda (row)
                   (let ([newobject (make class)])
                     (for-each (lambda (column model-column)
                                 (set! (slot-value newobject model-column) column)) row (list-columns newobject))
                     newobject)) data))
        #f))
  )                                     ; End module
