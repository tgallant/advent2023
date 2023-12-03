;; https://adventofcode.com/2023/day/2

(require 'aoc)

(defun parse-game (parts)
  (cl-destructuring-bind (game moves) parts
    (-<>> (string-split game " " t)
          (nth 1)
          (string-to-number)
          (list <> moves))))

(defun parse-pick (str)
  (-<> (string-split str " " t)
       (cl-destructuring-bind (count color) <>
         (cons (intern color) (string-to-number count)))))

(defun parse-move (str)
  (->> (string-split str ", " t)
       (mapcar 'parse-pick)))

(defun parse-moves (parts)
  (cl-destructuring-bind (game moves) parts
    (->> (string-split moves "; " t)
         (mapcar 'parse-move)
         (list game))))

(defun make-game (str)
  (->> (string-split str ": " t)
       (parse-game)
       (parse-moves)))

(defconst color-mapping '(red 12 green 13 blue 14))

(defun truep (val)
  (eq t val))

(defun check-color (color)
  (cl-destructuring-bind (val . count) color
    (<= count (plist-get color-mapping val))))

(defun check-move (move)
  (cl-every 'truep (mapcar 'check-color move)))

(defun determine-possibility (game)
  (cl-destructuring-bind (id moves) game
    (cons id (cl-every 'truep (mapcar 'check-move moves)))))

(defun collect-possible-games (acc cur)
  (cl-destructuring-bind (game . possible) cur
    (if possible (append acc (list game)) acc)))

(defun 2023-02-part1 (input)
  (-<>> (mapcar 'make-game input)
        (mapcar 'determine-possibility)
        (cl-reduce 'collect-possible-games <> :initial-value '())
        (apply '+)))

(defun check-minimum (acc cur)
  (cl-destructuring-bind (color . count) cur
    (cond ((eq nil (plist-get acc color))
           (plist-put acc color count))
          ((> count (plist-get acc color))
           (plist-put acc color count))
          (t acc))))

(defun update-minimum (acc cur)
  (cl-reduce 'check-minimum cur :initial-value acc))

(defun determine-minimums (game)
  (cl-destructuring-bind (id moves) game
    (cl-reduce 'update-minimum moves :initial-value '())))

(defun collect-power-score (minimums)
  (cl-reduce '* (cl-remove-if-not 'numberp minimums)))

(defun 2023-02-part2 (input)
  (-<>> (mapcar 'make-game input)
        (mapcar 'determine-minimums)
        (mapcar 'collect-power-score)
        (apply '+)))

(defconst testfile (expand-file-name "02.test.txt"))
(defconst inputfile (expand-file-name "02.input.txt"))

(defcheck 2023-02-part1 testfile 8)
(defcheck 2023-02-part1 inputfile 2476)
(defcheck 2023-02-part2 testfile 2286)
(defcheck 2023-02-part2 inputfile 54911)

(solve "2023-02")
