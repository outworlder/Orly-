(use coops)
(use coops-primitive-objects)
(use ssql)
(use sql-de-lite)

(load "database-adapter")

;; (define-model (<name> <parent>)
;;   table: "name"                         ; Optional
;;   columns: ()
;;   has-many: (<another-model>)
;;   belongs-to: (<model> foreign-key: 'asdf)
;;   has-and-belongs-to-many: ())

;; (callback <model> 'before 'save BODY)

;; (define-model '(<inventory-categories> <model-base>)
;;   table: "invcategories"
;;   columns: '(categoryID categoryName description iconID published)
;;   has-many: '(<inventory-groups> foreign-key: categoryID))

(define-class <model-base> ()
  ((persisted #f)
   (connection #f)
   (table #f)
   (attributes '())))

(define-generic (search <model-base>))
(define-generic (search-all <model-base>))
(define-generic (save! <model-base>))
(define-generic (destroy! <model-base>))
(define-generic (check-attributes <model-base>))
(define-generic (map-attributes <model-base>))
(define-generic (set-attribute <model-base> <symbol> value))
(define-generic (get-attribute <model-base> <symbol>))
(define-generic (set-attributes <model-base> <list>))

(define-method (check-attributes (model <model-base>))
  (if (null? (slot-value model 'attributes))
      (print "NO ATTRIBUTES DEFINED!"))
  (if (null? (slot-value model 'table))
      (print "NO TABLE DEFINED!")))

(define-method (search (model <model-base>))
  (let ((connection (slot-value model 'connection)))
    (if connection
        (let ((results
               (db-fetch (slot-value model 'connection)
                         (ssql->sql #f `(select ,(get-columns model)
                                                (from ,(slot-value model 'table)))))))
          (print "Results: " results)
          (map (lambda (item)
                 (print "Item: " item)
                 ;; This stinks. We need to be able to create any model
                 (let ((new-model (make <model-base>)))
                   (set-attributes model item)
                   new-model)) results)))))

(define-method (search before: (model <model-base>))
  (check-attributes model))

(define-method (save (model <model-base>))
  (let ((connection (slot-value model 'connection)))
    (if connection
        (let ((result
               (db-execute connection (if (slot-value model 'persisted)
                                          (update model)
                                          (insert model)))))
          (if (eq? result 1)
              (set! (slot-value model 'persisted) #t)
              result)))))

(define-method (insert (model <model-base>))
  (ssql->sql #f `(insert (into ,(slot-value model 'table))
                         ,(get-columns model)
                         ,(get-insert-values model))))

(define-method (update (model <model-base>))
  (ssql->sql #f `(update (table ,(slot-value model 'table))
                         (set ,@(slot-value model 'attributes)))))

(define-method (set-attribute (model <model-base>) (name <symbol>) value)
  (let ((data (assoc name (slot-value model 'attributes))))
    (set-cdr! data value)
    value))

(define-method (get-attribute (model <model-base>) (name <symbol>))
  (let ((data (cdr (assoc name (slot-value model 'attributes)))))
    data))

(define-method (set-attributes (model <model-base>) (attributes <list>))
  (for-each (lambda (x)
         (set-attribute model (car x) (cadr x))) attributes))

;;; This should not be exported
(define-method (get-update-columns (model <model-base>))
  `(columns ,@(map (lambda (attr)
                     `(=,(car attr) ,(cdr attr))) (slot-value model 'attributes))))

(define-method (get-columns (model <model-base>))
  `(columns ,@(map (lambda (attr)
                    (car attr)) (slot-value model 'attributes))))

(define-method (get-insert-values (model <model-base>))
  `(values ,@(map (lambda (attr)
                    (cdr attr)) (slot-value model 'attributes))))



  ;; ;;; Without any macros
;; (define-class <inventory-categories> <model-base>
;;   ((table "invcategories")
;;    (attributes (col1 col2 col3 col4))
;;    (has-many (<inventory-groups>))))

;; ;;; Functions
;; (search <inventory-categories> conditions: '(where (= id 1)))

;; (search-all <inventory-categories>)
