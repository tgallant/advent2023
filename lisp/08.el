;; https://adventofcode.com/2023/day/8

(require 'aoc2023)

(defun make-directions (str)
  (string-split str "" t))

(defun make-map-entry (str)
  (list (substring str 0 3)
        (substring str 7 10)
        (substring str 12 15)))

(defun add-map-entry (tbl str)
  (let ((entry (make-map-entry str)))
    (puthash (car entry) (cdr entry) tbl)))

(defun make-table (lst)
  (let ((tbl (make-hash-table :test 'equal)))
    (mapc (lambda (elt) (add-map-entry tbl elt)) lst)
    tbl))

(defun make-map (lst)
  (cons
   (make-directions (car lst))
   (make-table (cdr lst))))

(defun traverse-map (map)
  (setq dir (car map))
  (setq tbl (cdr map))
  (setq len (length dir))
  (setq count 0)
  (setq key "AAA")
  (setq steps '())
  (defun should-continue ()
    (not (equal key "ZZZ")))
  (defun next ()
    (let ((val (gethash key tbl))
          (ins (nth (mod count len) dir)))
      (if (equal ins "L")
          (nth 0 val)
        (nth 1 val))))
  (defun do-step ()
    (push key steps)
    (setq key (next))
    (cl-incf count))
  (while (should-continue)
    (do-step))
  steps)

(defun 2023-08-part1 (input)
  (-<>> (make-map input)
        (traverse-map)
        (length)))

(defconst testfile (expand-file-name "input/08.test.txt"))
(defconst inputfile (expand-file-name "input/08.input.txt"))

(defcheck 2023-08-part1 testfile 6)
(defcheck 2023-08-part1 inputfile 17621)

(solve "2023-08")
