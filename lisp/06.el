;; https://adventofcode.com/2023/day/6

(require 'aoc2023)

(defun make-number-list (str)
  (-<>> (string-split str ":" t)
        (nth 1)
        (string-split <> " " t)
        (mapcar 'string-to-number)))

(defun make-races (lst)
  (cl-mapcar 'cons
             (make-number-list (nth 0 lst))
             (make-number-list (nth 1 lst))))

(defun score-race (race)
  (cl-destructuring-bind (distance . record) race
    (-<>> (cl-mapcar 'list
                     (number-sequence 0 distance)
                     (number-sequence distance 0 -1))
          (mapcar (lambda (p) (apply '* p)))
          (cl-remove-if (lambda (elt) (<= elt record)))
          (length))))

(defun 2023-06-part1 (input)
  (-<>> (make-races input)
        (mapcar 'score-race)
        (apply '*)))

(defconst testfile (expand-file-name "input/06.test.txt"))
(defconst inputfile (expand-file-name "input/06.input.txt"))

(defcheck 2023-06-part1 testfile 288)
(defcheck 2023-06-part1 inputfile 633080)

(solve "2023-06")
