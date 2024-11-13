;; https://adventofcode.com/2023/day/4

(require 'aoc2023)

(defun parse-numbers (str)
  (string-split str " " t))

(defun parse-card (parts)
  (list
   (parse-numbers (nth 0 parts))
   (parse-numbers (nth 1 parts))))

(defun make-card (str)
  (-<>> (string-split str ": " t)
        (nth 1)
        (string-split <> " | " t)
        (parse-card)))

(defun determine-winnings (card)
  (defun collect-winnings (acc cur)
    (let ((win (member cur (nth 0 card))))
      (if win (append acc (list cur))
        acc)))
  (cl-reduce 'collect-winnings (nth 1 card) :initial-value '()))

(defun score-winnings (winnings)
  (let ((len (length winnings)))
    (cond ((eq 0 (length winnings)) 0)
          (t (expt 2 (- len 1))))))

(defun 2023-04-part1 (input)
  (-<>> (mapcar 'make-card input)
        (mapcar 'determine-winnings)
        (mapcar 'score-winnings)
        (apply '+)))

(defun score-winnings-v2 (winnings)
  (defun depth-count (lst)
    (cond ((eq lst nil) 0)
          (t (+ (count-card 0 (car lst)) (depth-count (cdr lst))))))
  (defun count-card (acc cur)
    (+ acc 1 (depth-count (number-sequence (+ 1 cur) (+ cur (nth cur winnings))))))
  (cl-reduce 'count-card
             (number-sequence 0 (- (length winnings) 1))
             :initial-value 0))

(defun 2023-04-part2 (input)
  (-<>> (mapcar 'make-card input)
        (mapcar 'determine-winnings)
        (mapcar 'length)
        (score-winnings-v2)))

(defconst testfile (expand-file-name "input/04.test.txt"))
(defconst inputfile (expand-file-name "input/04.input.txt"))

(defcheck 2023-04-part1 testfile 13)
(defcheck 2023-04-part1 inputfile 18653)
(defcheck 2023-04-part2 testfile 30)
(defcheck 2023-04-part2 inputfile 5921508)

(solve "2023-04")
