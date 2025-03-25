;;; Global Variables
(defvar *input* nil)  ; Input string
(defvar *pos* 0)      ; Position tracker

;;; Helper Functions
(defun peek-next ()
  "Returns the current character without consuming it. Returns #\0 when end of string is reached."
  (if (< *pos* (length *input*)) 
      (char *input* *pos*) 
      #\0)) ; Return null character (sentinel) if at end of string

(defun next-char ()
  "Consumes and returns the next character."
  (if (< *pos* (length *input*))
      (prog1 (char *input* *pos*) (incf *pos*))
      nil))

(defun match (expected)
  "Matches the expected character and advances if matched."
  (if (char= (peek-next) expected)
      (progn (next-char) t)
      nil))

;;; Recursive Descent Parsing Functions
(defun parse-I ()
  "Parses I → iES."
  (and (match #\i) (parse-E) (parse-S)))

(defun parse-E ()
  "Parses E → G E'."
  (and (parse-G) (parse-E-prime)))

(defun parse-E-prime ()
  "Parses E' → oG E' | ε."
  (if (match #\o)
      (and (parse-G) (parse-E-prime))
      t))  ; ε case (do nothing)

(defun parse-G ()
  "Parses G → x | y | z | w."
  (or (match #\x) (match #\y) (match #\z) (match #\w)))

(defun parse-S ()
  "Parses S → s | dLb."
  (or (match #\s) 
      (and (match #\d) (parse-L) (match #\b))))

(defun parse-L ()
  "Parses L → s L'."
  (and (match #\s) (parse-L-prime)))

(defun parse-L-prime ()
  "Parses L' → s L' | ε."
  (if (match #\s)
      (parse-L-prime)
      t))  ; ε case (do nothing)

;;; Main Parser Function
(defun parse (input)
  "Parses the given input string and checks if it is valid."
  (setq *input* input)
  (setq *pos* 0)
  (let ((result (and (parse-I) (= *pos* (length *input*)))))
    ;; Reset input state after parsing attempt
    (setq *input* nil)
    (setq *pos* 0)
    (if result
        (format t "Success: The string '~A' is valid.~%" input)
        (format t "Failure: The string '~A' is invalid.~%" input))))

;;; Test Cases
(setq valid-strings '("ixoys" "ixoyowdssb" "ixozs" "ixoydssb" "iwoydssb" "ixoydssssb" "ixozs"))
(setq invalid-strings '("xoys" "ixowdb" "ixzy" "iwyd" "ixoysx" "ixozsb" "ixoyd"))

(defun test-parser ()
  "Runs the parser on valid and invalid strings."
  (format t "Testing valid strings:~%")
  (dolist (s valid-strings)
    (format t "Testing: '~A' (Expected: Valid) -> " s)
    (parse s))
  
  (format t "~%Testing invalid strings:~%")
  (dolist (s invalid-strings)
    (format t "Testing: '~A' (Expected: Invalid) -> " s)
    (parse s)))

;;; Run Tests Automatically
(test-parser)
