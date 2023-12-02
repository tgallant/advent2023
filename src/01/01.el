;; https://adventofcode.com/2023/day/1

(require 'aoc)

(defun join-first-and-last-num (lst)
  (->> (list (number-to-string (car lst))
             (number-to-string (car (last lst))))
       (string-join)
       (string-to-number)))

(defun double-first-num (lst)
  (->> (list (number-to-string (car lst))
             (number-to-string (car lst)))
       (string-join)
       (string-to-number)))

(defun determine-calibration (lst)
  (cond ((= 1 (length lst)) (double-first-num lst))
        (t (join-first-and-last-num lst))))

(defun collect (acc cur)
  (let ((num (string-to-number cur)))
    (cond ((equal cur "0") (append acc (list 0)))
          ((> num 0) (append acc (list num)))
          (t acc))))

(defun collect-numbers (lst)
  (cl-reduce 'collect lst :initial-value '()))

(defun collect-v2 (acc cur)
  (let* ((num (string-to-number cur))
         (lst (car acc))
         (buf (cdr acc))
         (nxt (string-join (list buf cur))))
    (cond ((equal cur "0")
           (append acc (list 0)))
          ((> num 0)
           (cons (append lst (list num)) nil))
          ((cl-search "one" nxt)
           (cons (append lst (list 1)) "e"))
          ((cl-search "two" nxt)
           (cons (append lst (list 2)) "o"))
          ((cl-search "three" nxt)
           (cons (append lst (list 3)) "e"))
          ((cl-search "four" nxt)
           (cons (append lst (list 4)) nil))
          ((cl-search "five" nxt)
           (cons (append lst (list 5)) "e"))
          ((cl-search "six" nxt)
           (cons (append lst (list 6)) nil))
          ((cl-search "seven" nxt)
           (cons (append lst (list 7)) "n"))
          ((cl-search "eight" nxt)
           (cons (append lst (list 8)) "t"))
          ((cl-search "nine" nxt)
           (cons (append lst (list 9)) "e"))
          (t (cons lst nxt)))))

(defun collect-numbers-v2 (lst)
  (car (cl-reduce 'collect-v2 lst :initial-value '())))

(defun split (str)
  (split-string str "" t))

(defsolution part1
  (mapcar 'split)
  (mapcar 'collect-numbers)
  (mapcar 'determine-calibration)
  (sum))

(defsolution part2
  (mapcar 'split)
  (mapcar 'collect-numbers-v2)
  (mapcar 'determine-calibration)
  (sum))

(defsolve "2023-01"
  ((part1 "01.test.txt") 142)
  ((part1 "01.input.txt") 54927)
  ((part2 "01.test.pt2.txt") 281)
  ((part2 "01.input.txt") 54581))
