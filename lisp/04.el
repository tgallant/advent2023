;; https://adventofcode.com/2023/day/4

(require 'aoc2023)

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

(defun 2023-04-part1 (input)
  (-<>> (mapcar 'make-game input)
        (mapcar 'determine-possibility)
        (cl-reduce 'collect-possible-games <> :initial-value '())
        (apply '+)))

(defconst testfile (expand-file-name "input/04.test.txt"))
;; (defconst inputfile (expand-file-name "04.input.txt"))

(defcheck 2023-04-part1 testfile 13)

(solve "2023-04")
