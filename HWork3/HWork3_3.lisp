(defun partition-list (unprocessed list1 list2)
(cond
;; Case 1 If unprocessed is empty, return the two lists
((null unprocessed) (list list1 list2))
;; Case 2 If there is only one item, add it to the first list
((null (cdr unprocessed)) (list (cons (car unprocessed) list1) list2))
;; Case 3 Recursive step: Take the first two items, add one to each list
(t (partition-list (cddr unprocessed) (cons (car unprocessed) list1) (cons (cadr unprocessed) list2)))))




;; Function to sort a pair of numbers
(defun sort-pair (a b)
  (if (< a b)
      (list a b)
      (list b a)))

;; Function to create initial sorted pairs with step tracking
(defun make-sorted-pairs (lst)
  (labels ((make-pairs-step (processed remaining)
             (cond ((null remaining)
                    processed)
                   ((null (cdr remaining))
                    (format t "Current pairs: ~a~%" processed)
                    (format t "Remaining: ~a~%" remaining)
                    (cons (list (car remaining)) processed))
                   (t
                    (let ((new-pair (sort-pair (car remaining) (cadr remaining))))
                      (format t "Current pairs: ~a~%" processed)
                      (format t "Remaining: ~a~%~%" remaining)
                      (make-pairs-step 
                       (cons new-pair processed)
                       (cddr remaining)))))))
    (reverse (make-pairs-step nil lst))))

;; Function to merge two sorted lists
(defun merge-sorted-lists (list1 list2)
  (cond ((null list1) list2)
        ((null list2) list1)
        ((< (car list1) (car list2))
         (cons (car list1) (merge-sorted-lists (cdr list1) list2)))
        (t
         (cons (car list2) (merge-sorted-lists list1 (cdr list2))))))

;; Function to merge pairs of sorted lists
(defun merge-pass (lists)
  (cond ((null lists) nil)
        ((null (cdr lists)) lists)
        (t (cons (merge-sorted-lists (car lists) (cadr lists))
                (merge-pass (cddr lists))))))

;; Main mergesort function that shows steps
(defun mergesort-with-steps (lst)
  (format t "Starting with list: ~a~%~%" lst)
  ;; Step 1: Create sorted pairs with detailed steps
  (let ((pairs (make-sorted-pairs lst)))
    (format t "~%Final sorted pairs: ~a~%~%" pairs)
    
    ;; Step 2: Merge pairs until we have one sorted list
    (do ((current-lists pairs (merge-pass current-lists)))
        ((null (cdr current-lists)) (car current-lists))
      (format t "After merge pass: ~a~%~%" (merge-pass current-lists)))))
