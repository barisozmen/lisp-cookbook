(defmacro define-ui-component (name params &body body)
  "Define a reusable UI component."
  `(defmacro ,name ,params
     `(generate-html-with-jquery ,',body)))

(defmacro generate-html-with-components (structure)
  "Generate HTML with reusable UI components."
  (if (atom structure)
      structure
      (let* ((tag (car structure))
             (attrs (if (and (listp (cadr structure)) (listp (car (cadr structure))))
                        nil
                        (cadr structure)))
             (children (if attrs
                           (cddr structure)
                           (cdr structure))))
        (if (symbolp tag)  ; Check if the tag is a macro/component
            `(,tag ,@attrs ,@children)
            `(concat
              "<" ,(symbol-name tag)
              ,(if attrs
                   `(concat
                     " "
                     (mapconcat (lambda (attr)
                                  (if (or (eq (car attr) :on-click)
                                          (eq (car attr) :animate)
                                          (eq (car attr) :ajax)
                                          (eq (car attr) :add-class)
                                          (eq (car attr) :remove-class))
                                      ""
                                      (concat (symbol-name (car attr)) "=\"" (cdr attr) "\"")))
                                ',attrs
                                " "))
                   "")
              ">"
              ,@(mapcar (lambda (child)
                          `(generate-html-with-components ,child)) children)
              "</" ,(symbol-name tag) ">")))))

(define-ui-component ui-button (label class on-click)
  `(button (:class ,class :on-click ,on-click)
           ,label))

(generate-html-with-components
 '(div (:class "button-container")
       (ui-button "Click Me" "btn-primary" "alert('Button 1 clicked!')")
       (ui-button "Delete" "btn-danger" "alert('Delete clicked!')")))

(define-ui-component ui-card (title content footer class)
  `(div (:class (concat "card " ,class))
        (h3 (:class "card-title") ,title)
        (p (:class "card-content") ,content)
        (div (:class "card-footer") ,footer)))

(generate-html-with-components
 '(div (:class "card-container")
       (ui-card "Card 1" "This is the first card." "Footer 1" "card-primary")
       (ui-card "Card 2" "Another card with content." "Footer 2" "card-secondary")))


(define-ui-component ui-modal (id title body footer on-open)
  `(div (:class "modal" :id ,id)
        (div (:class "modal-header")
             (h3 nil ,title))
        (div (:class "modal-body")
             ,body)
        (div (:class "modal-footer")
             ,footer)
        (script nil
                (concat "$('#" ,id "').on('show', function() {" ,on-open "});"))))


(generate-html-with-components
 '(div (:class "page")
       (ui-modal "myModal" "Modal Title"
                 "This is the modal body."
                 "Modal footer content."
                 "console.log('Modal opened!');")))

