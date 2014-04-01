(use coops)
(use coops-primitive-objects)
(use ssql)
(use sql-de-lite)

(define orly-database-url (make-parameter #f))
(define orly-debug (make-parameter #f))

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
  ((connection #f)
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
  (fetch (slot-value model 'connection)
         (ssql->sql #f `(select (attributes ,@(slot-value model 'attributes))
                          (from ,(slot-value model 'table))))))

(define-method (search before: (model <model-base>))
  (check-attributes model))

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

  ;; ;;; Without any macros
;; (define-class <inventory-categories> <model-base>
;;   ((table "invcategories")
;;    (attributes (col1 col2 col3 col4))
;;    (has-many (<inventory-groups>))))

;; ;;; Functions
;; (search <inventory-categories> conditions: '(where (= id 1)))

;; (search-all <inventory-categories>)
