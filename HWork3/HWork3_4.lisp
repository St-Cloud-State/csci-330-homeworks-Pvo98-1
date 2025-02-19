(defun insert-item (item sorted) 
(cond 
((null sorted) (list item)) ; If the sorted list is empty, return the item as the only element. 
((< item (car sorted)) (cons item sorted)) ; If the item is smaller than the first element, insert it at the front. 
(t (cons (car sorted) (insert-item item (cdr sorted)))))) ; recursively insert into the rest of the list. 

(defun insertion-sort (unsorted sorted) 
(cond 
((null unsorted) sorted) ; If unsorted list is empty, return the sorted list. 
((null (cdr unsorted)) (insert-item (car unsorted) sorted)) ; If unsorted list has only one item, insert it into the Sorted List. 
(t (insertion-sort (cdr unsorted) (insert-item (car unsorted) sorted))))) ; Insert the first item into sorted list. 

(defun sort-list (lst) 
(insertion-sort lst nil)) ; Start sorting with an empty sorted list.
