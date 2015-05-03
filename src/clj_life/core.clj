(ns clj-life.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clj-life.life :as l])
  (:gen-class))

(defn grid->draw
  [g size]
  (apply concat
         (for [y (range (count g))]
           (for [x (range (count (g y)))]
             {:x (* x size)
              :y (* y size)
              :state (l/get-cell g x y)}))))

(defn setup
  []
  (q/frame-rate 30)
  (q/background 0)
  {:grid (-> (l/make-grid 80 60)
             (l/paste-pattern 0 0 (:glider l/patterns))
             (l/paste-pattern 20 0 (:glider l/patterns))
             (l/paste-pattern 40 0 (:glider l/patterns))
             (l/paste-pattern 50 40 (:block l/patterns)))
   :generation 1
   :rate 5
   :paused false
   :paint false})

(defn update-state
  [state]
  (if (and (not (:paused state))
           (= (mod (q/frame-count) (:rate state)) 0))
    (assoc state
           :grid (l/step-grid (:grid state))
           :generation (inc (:generation state)))
    state))

(defn key-handler
  [state event]
  (println event)
  (let [key? #(= (:key event) %)]
    (cond
      (key? :left) (let [new (inc (:rate state))]
                     (assoc state :rate new))
      (key? :right) (let [new (dec (:rate state))]
                      (if (>= new 1)
                        (assoc state :rate new)
                        state))
      (or (key? :c) (key? :C)) (assoc state
                                      :grid (l/make-grid 80 58)
                                      :generation 0)
      (or (key? :p) (key? :P)) (assoc state :paused (not (:paused state)))
      (or (key? :l) (key? :L)) (assoc state :paint (not (:paint state)))
      :else state)))

(defn toggle-at-mouse
  [state event]
  (if (> (:y event) 20)
    (let [x (int (/ (:x event) 10))
          y (- (int (/ (:y event) 10)) 2)]
      (assoc state :grid (l/toggle-cell (:grid state) x y)))
    state))

(defn do-paint
  [state event]
  (if (:paint state)
    (toggle-at-mouse state event)
    state))

(defn draw-state
  [state]
  (q/background 0)
  (q/fill 255 255 255)
  (q/rect 0 0 800 20)
  (q/fill 0 0 0)
  (q/text "Slower: ← / Faster: → / Pause: P / Clear: C / Paint: L" 516 15)
  (if (:paint state)
    (q/text "==PAINTING==" (- (/ (q/width) 2) 90) 15))
  (if (:paused state)
    (q/text "==PAUSED==" (+ (/ (q/width) 2) 5) 15))
  (q/text (str "Gen: " (:generation state)) 5 15)
  (q/fill 255 255 255)
  (q/with-translation [5 25]
    (doall
     (for [cell (grid->draw (:grid state) 10)
           :when (true? (:state cell))]
       (q/ellipse (:x cell) (:y cell) 10 10)))))

(defn run-game
  []
  (q/defsketch clj-life
    :title "Conway's Game of Life"
    :size [800 610]
    :setup setup
    :update update-state
    :draw draw-state
    :key-pressed key-handler
    :mouse-clicked toggle-at-mouse
    :mouse-moved do-paint
    :middleware [m/fun-mode]))

(defn -main
  [& args]
  (run-game))
