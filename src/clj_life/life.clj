(ns clj-life.life)

;; The universe of the Game of Life is an infinite two-dimensional orthogonal
;; grid of square cells, each of which is in one of two possible states, alive
;; or dead. Every cell interacts with its eight neighbours, which are the
;; cells that are horizontally, vertically, or diagonally adjacent. At each
;; step in time, the following transitions occur:
;;
;; 1. Any live cell with fewer than two live neighbours dies, as if caused
;;    by under-population.
;; 2. Any live cell with two or three live neighbours lives on to the next
;;    generation.
;; 3. Any live cell with more than three live neighbours dies, as if by
;;    overcrowding.
;; 4. Any dead cell with exactly three live neighbours becomes a live
;;    cell, as if by reproduction.
;;
;; The initial pattern constitutes the seed of the system. The first
;; generation is created by applying the above rules simultaneously to every
;; cell in the seed-births and deaths occur simultaneously, and the discrete
;; moment at which this happens is sometimes called a tick (in other words,
;; each generation is a pure function of the preceding one). The rules
;; continue to be applied repeatedly to create further generations.

(def patterns
  {:block [[0 0 0 0]
           [0 1 1 0]
           [0 1 1 0]
           [0 0 0 0]]
   :beehive [[0 0 0 0 0 0]
             [0 0 1 1 0 0]
             [0 1 0 0 1 0]
             [0 1 0 0 1 0]
             [0 0 1 1 0 0]
             [0 0 0 0 0 0]]
   :blinker [[0 0 0 0 0]
             [0 0 1 0 0]
             [0 0 1 0 0]
             [0 0 1 0 0]
             [0 0 0 0 0]]
   :toad [[0 0 0 0 0 0]
          [0 0 0 0 0 0]
          [0 0 1 1 1 0]
          [0 1 1 1 0 0]
          [0 0 0 0 0 0]
          [0 0 0 0 0 0]]
   :glider [[0 0 0 0 0]
            [0 0 1 0 0]
            [0 0 0 1 0]
            [0 1 1 1 0]
            [0 0 0 0 0]]})

(defn make-grid
  [x y]
  (into [] (repeat y (into [] (repeat x false)))))

(defn get-cell
  [g x y]
  ((g y) x))

(defn set-cell
  [g x y val]
  (let [row (g y)]
    (assoc-in g [y x] val)))

(defn heal-cell
  [g x y]
  (set-cell g x y true))

(defn kill-cell
  [g x y]
  (set-cell g x y false))

(defn toggle-cell
  [g x y]
  (set-cell g x y (not (get-cell g x y))))

(defn count-cell
  [g x y]
  (try
    (let [cell (get-cell g x y)]
      (if cell 1 0))
    (catch IndexOutOfBoundsException e 0)))

(defn count-neighbours
  [g x y]
  (let [to-check [[-1 -1] [0  -1] [1  -1]
                  [-1  0]         [1   0]
                  [-1  1] [0   1] [1   1]]]
    (apply + (map #(count-cell g (+ x (% 0)) (+ y (% 1)))
                  to-check))))

(defn step-cell
  [g x y]
  (let [cell (get-cell g x y)
        n (count-neighbours g x y)]
    (if cell
      (cond
        (< n 2) false ; rule 1
        (<= 2 n 3) true ; rule 2
        :else false) ; rule 3
      (if (= n 3) ; rule 4
        true false))))

(defn step-row
  [g y]
  (let [x-len (count (g y))]
    (loop [updated-row (g y), x 0]
      (if (< x x-len)
        (recur (assoc updated-row x (step-cell g x y))
               (inc x))
        updated-row))))

(defn step-grid
  [g]
  (let [y-len (count g)]
    (loop [updated g, y 0]
      (if (< y y-len)
        (let [row (step-row g y)]
          (recur (assoc updated y row) (inc y)))
        updated))))

(defn pattern->grid
  [pattern & {:keys [x y] :or {x 0, y 0}}]
  (apply concat
         (for [cur-y (range (count pattern))]
           (for [cur-x (range (count (pattern cur-y)))]
             [(+ cur-x x)
              (+ cur-y y)
              (if (= (get-in pattern [cur-y cur-x]) 1)
                true
                false)]))))

(defn paste-pattern
  [g x y pattern]
  (reduce #(apply set-cell %1 %2) g (pattern->grid pattern :x x :y y)))

(defn print-grid
  [g]
  (let [y-len (count g)
        x-len (count (g 0))]
    (printf "+%s+\n" (apply str (repeat x-len "--")))
    (doall
     (for [y (range y-len)]
       (do (print "|")
           (doall
            (for [x (range x-len)]
              (let [cell (get-cell g x y)]
                (print (if cell "()" "  ")))))
           (printf "|\n"))))
    (printf "+%s+\n" (apply str (repeat x-len "--")))))
