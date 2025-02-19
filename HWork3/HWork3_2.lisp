;; Function to merge two sorted lists
(defun merge-sorted-lists (list1 list2)
  (cond ((null list1) list2)
        ((null list2) list1)
        ((< (car list1) (car list2))
         (cons (car list1) (merge-sorted-lists (cdr list1) list2)))
        (t
         (cons (car list2) (merge-sorted-lists list1 (cdr list2))))))

;; Function to sort a list using merge sort
(defun mergesort (lst)
  (cond ((null lst) nil)                    ; empty list
        ((null (cdr lst)) lst)              ; single element
        (t (let* ((partitioned (partition-list lst))
                  (left (car partitioned))
                  (right (cadr partitioned)))
             (merge-sorted-lists (mergesort left)    ; recursively sort left
                               (mergesort right)))))) ; recursively sort right

;; Function to partition a list into two sublists
(defun partition-list (lst)
  (labels ((partition-step (unprocessed list1 list2)
            (cond ((null unprocessed) 
                   (list list1 list2))  ; Return both lists when no more items
                  ((null (cdr unprocessed))  ; Only one item left
                   (partition-step nil 
                                 (append list1 (list (car unprocessed)))
                                 list2))
                  (t  ; Two or more items - add to each list
                   (partition-step (cddr unprocessed)
                                 (append list1 (list (car unprocessed)))
                                 (append list2 (list (cadr unprocessed))))))))
    (format t "Starting partition of: ~a~%" lst)
    (let ((result (partition-step lst nil nil)))
      (format t "Partitioned into:~%List 1: ~a~%List 2: ~a~%~%" 
              (car result) (cadr result))
      result)))
