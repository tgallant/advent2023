;; https://adventofcode.com/2023/day/3

(require 'aoc2023)

(defconst digits '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))

(defun check-character (acc cur)
  (cl-destructuring-bind (lst pos reg buf) acc
    (cond ((cl-find cur digits :test 'equal)
           (list lst
                 (1+ pos)
                 (append reg (list pos))
                 (string-join (list buf cur))))
          ((and buf (equal cur ""))
           (list (append lst (list (cons reg (string-to-number buf))))
                 (1+ pos)
                 nil nil))
          (buf
           (list (append lst (list (cons reg (string-to-number buf))))
                 (1+ pos)
                 nil nil))
          (t (list lst (1+ pos) reg buf)))))

(defun collect-number-positions (lst)
  (car (cl-reduce 'check-character lst :initial-value '(() 0 () nil))))

(defun symbp (val)
  (cond ((cl-find val digits :test 'equal) nil)
        ((equal val ".") nil)
        ((equal val "") nil)
        ((equal val nil) nil)
        (t t)))

(defun get-grid-val (grid pos)
  (cl-destructuring-bind (x . y) pos
    (cond ((< x 0) nil)
          ((< y 0) nil)
          (t (nth x (nth y grid))))))

(defun part-numberp (num row grid)
  (cl-loop for pos in num
           for le = (symbp (get-grid-val grid (cons (- pos 1) row)))
           for lu = (symbp (get-grid-val grid (cons (- pos 1) (- row 1))))
           for up = (symbp (get-grid-val grid (cons pos (- row 1))))
           for ur = (symbp (get-grid-val grid (cons (+ pos 1) (- row 1))))
           for ri = (symbp (get-grid-val grid (cons (+ pos 1) row)))
           for dr = (symbp (get-grid-val grid (cons (+ pos 1) (+ row 1))))
           for do = (symbp (get-grid-val grid (cons pos (+ row 1))))
           for dl = (symbp (get-grid-val grid (cons (- pos 1) (+ row 1))))
           if (cl-some (lambda (v) (eq v t)) (list le lu up ur ri dr do dl))
           return t
           finally return nil))

(defun check-row (acc cur)
  (cl-destructuring-bind (sum row grid) acc
    (cond ((part-numberp (car cur) row grid)
           (list (+ sum (cdr cur)) row grid))
          (t acc))))

(defun check-positions (acc cur)
  (cl-destructuring-bind (sum row grid) acc
    (list
     (car (cl-reduce 'check-row cur :initial-value (list sum row grid)))
     (1+ row)
     grid)))

(defun collect-part-numbers (grid)
  (-<>> (mapcar 'collect-number-positions grid)
        (cl-reduce 'check-positions <> :initial-value (list 0 0 grid))
        (car)))

(defun split (str)
  (split-string str ""))

(defun 2023-03-part1 (input)
  (->> (mapcar 'split input)
       (collect-part-numbers)))

(defun check-row-v2 (acc cur)
  (cl-destructuring-bind (sum row grid) acc
    (cond ((part-numberp (car cur) row grid)
           (list (+ sum (cdr cur)) row grid))
          (t acc))))

(defun check-positions-v2 (acc cur)
  (cl-destructuring-bind (sum row grid) acc
    (list
     (car (cl-reduce 'check-row cur :initial-value (list sum row grid)))
     (1+ row)
     grid)))

(defun make-adj (x y)
  (let ((le (cons (- x 1) y))
        (lu (cons (- x 1) (- y 1)))
        (up (cons x (- y 1)))
        (ur (cons (+ x 1) (- y 1)))
        (ri (cons (+ x 1) y))
        (dr (cons (+ x 1) (+ y 1)))
        (do (cons x (+ y 1)))
        (dl (cons (- x 1) (+ y 1))))
    (list le lu up ur ri dr do dl)))

(defun collect-adjacent (nums adj)
  (cl-loop for pos in adj
           for x = (car pos)
           for y = (cdr pos)
           append (cl-loop for col in (nth y nums)
                           if (cl-find x (car col))
                           collect (cdr col))))

(defun check-adj (adj)
  (let ((vals (cl-remove-duplicates adj)))
    (cond ((= (length vals) 2)
           (cl-reduce '* vals))
          (t 0))))

(defun calculate-gear-ratios (grid nums)
  (cl-loop for row in grid
           for y from 0
           sum (cl-loop for col in row
                        for x from 0
                        if (equal "*" (get-grid-val grid (cons x y)))
                        sum (check-adj (collect-adjacent nums (make-adj x y))))))

(defun collect-gear-ratios (grid)
  (-<>> (mapcar 'collect-number-positions grid)
        (calculate-gear-ratios grid)))

(defun 2023-03-part2 (input)
  (->> (mapcar 'split input)
       (collect-gear-ratios)))

(defconst testfile (expand-file-name "input/03.test.txt"))
(defconst inputfile (expand-file-name "input/03.input.txt"))

(defcheck 2023-03-part1 testfile 4361)
(defcheck 2023-03-part1 inputfile 539433)
(defcheck 2023-03-part2 testfile 467835)
(defcheck 2023-03-part2 inputfile 75847567)

(solve "2023-03")
