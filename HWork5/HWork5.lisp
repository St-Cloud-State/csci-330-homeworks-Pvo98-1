
;;; Global Variables
(defvar *input* nil)  ; Input string
(defvar *pos* 0)      ; Position tracker

;;; Helper Functions
(defun peek-next ()
  "Returns the current character without consuming it."
  (if (< *pos* (length *input*)) 
      (char *input* *pos*) 
      (code-char 0))) ; Return a sentinel character instead of nil

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

(defun error-msg (expected)
  "Prints an error message for an unexpected symbol."
  (if (peek-next)
      (format t "Error at position ~A: Expected '~A', but found '~A'.~%" *pos* expected (peek-next))
      (format t "Error at position ~A: Expected '~A', but reached end of input.~%" *pos* expected))
  nil)

;;; Recursive Descent Parsing Functions
(defun parse-I ()
  "Parses I → iES."
  (if (match #\i)
      (and (parse-E) (parse-S))
      (error-msg #\i)))

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
  (if (or (match #\x) (match #\y) (match #\z) (match #\w))
      t
      (error-msg "one of 'x', 'y', 'z', or 'w'")))

(defun parse-S ()
  "Parses S → s | dLb."
  (if (match #\s)
      t
      (if (match #\d)
          (and (parse-L) (if (match #\b) t (error-msg #\b)))
          (error-msg "s or d"))))

(defun parse-L ()
  "Parses L → s L'."
  (if (match #\s)
      (parse-L-prime)
      (error-msg #\s)))

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
