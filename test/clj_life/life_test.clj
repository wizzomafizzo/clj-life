(ns clj-life.life-test
  (:require [clojure.test :refer :all]
            [clj-life.life :refer :all]))

(def grids
  [[[true false]
    [false true]]
   [[false true false] ; blinker
    [false true false]
    [false true false]]
   [[false false false false] ; stable
    [false true  true  false]
    [false true  true  false]
    [false false false false]]
   [[true true  true]
    [true false true]
    [true true  true]]])

(deftest test-cells
  (testing "get-cell"
    (is (= (get-cell (grids 0) 0 0) true))
    (is (= (get-cell (grids 0) 1 0) false))
    (is (= (get-cell (grids 0) 1 0) false))
    (is (= (get-cell (grids 0) 1 1) true))
    (is (thrown? IndexOutOfBoundsException (get-cell (grids 0) 0 -1)))
    (is (thrown? IndexOutOfBoundsException (get-cell (grids 0) 2 0))))
  (testing "set-cell"
    (is (= (get-cell (set-cell (grids 0) 0 0 true) 0 0) true))
    (is (= (get-cell (set-cell (grids 0) 0 0 false) 0 0) false))
    (is (= (get-cell (set-cell (grids 0) 1 1 true) 1 1) true))
    (is (= (get-cell (set-cell (grids 0) 1 1 false) 1 1) false))
    (is (thrown? IndexOutOfBoundsException (set-cell (grids 0) -1 -1 true)))
    (is (thrown? IndexOutOfBoundsException (set-cell (grids 0) 2 2 false ))))
  (testing "heal-cell"
    (is (= (get-cell (heal-cell (grids 0) 0 0) 0 0) true))
    (is (= (get-cell (heal-cell (grids 0) 1 1) 1 1) true))
    (is (= (get-cell (heal-cell (grids 0) 0 1) 0 1) true)))
  (testing "kill-cell"
    (is (= (get-cell (kill-cell (grids 0) 0 0) 0 0) false))
    (is (= (get-cell (kill-cell (grids 0) 1 1) 1 1) false))
    (is (= (get-cell (kill-cell (grids 0) 0 1) 0 1) false)))
  (testing "count-cell"
    (is (= (count-cell (grids 0) 0 0) 1))
    (is (= (count-cell (grids 0) 1 0) 0))
    (is (= (count-cell (grids 0) 0 1) 0))
    (is (= (count-cell (grids 0) 1 1) 1)))
  (testing "step-cell"
    (is (= (step-cell (grids 0) 0 0) false))
    (is (= (step-cell (grids 0) 0 0) false))
    (is (= (step-cell (grids 1) 0 0) false))
    (is (= (step-cell (grids 1) 0 1) true))
    (is (= (step-cell (grids 1) 1 1) true))
    (is (= (step-cell (grids 1) 2 1) true))
    (is (= (step-cell (grids 2) 0 0) false))
    (is (= (step-cell (grids 2) 1 1) true))
    (is (= (step-cell (grids 2) 2 2) true))
    (is (= (step-cell (grids 2) 3 3) false)))
  (testing "toggle-cell"
    (is (= (get-cell (toggle-cell (grids 0) 0 0) 0 0) false))
    (is (= (get-cell (toggle-cell (grids 0) 1 0) 1 0) true))
    (is (= (get-cell (toggle-cell (grids 0) 0 1) 0 0) true))
    (is (= (get-cell (toggle-cell (grids 0) 1 1) 1 1) false))))

(deftest test-grid
  (testing "make-grid"
    (is (= (make-grid 2 2)
          [[false false]
           [false false]]))
    (is (= (make-grid 3 3)
           [[false false false]
            [false false false]
            [false false false]]))
    (is (= (make-grid 2 3)
           [[false false]
            [false false]
            [false false]]))
    (is (= (make-grid 3 2)
           [[false false false]
            [false false false]])))
  (testing "count-neighbours"
    (is (= (count-neighbours (grids 0) 0 0) 1))
    (is (= (count-neighbours (grids 0) 1 0) 2))
    (is (= (count-neighbours (grids 1) 1 1) 2))
    (is (= (count-neighbours (grids 2) 0 0) 1))
    (is (= (count-neighbours (grids 2) 1 1) 3))
    (is (= (count-neighbours (grids 3) 1 1) 8)))
  (testing "step-row"
    (is (= (step-row (grids 0) 0) [false false]))
    (is (= (step-row (grids 0) 0) [false false]))
    (is (= (step-row (grids 1) 0) [false false false]))
    (is (= (step-row (grids 1) 1) [true true true]))
    (is (= (step-row (grids 1) 2) [false false false]))
    (is (= (step-row (grids 2) 0) [false false false false]))
    (is (= (step-row (grids 2) 1) [false true true false]))
    (is (= (step-row (grids 2) 2) [false true true false]))
    (is (= (step-row (grids 2) 3) [false false false false])))
  (testing "step-grid"
    (is (= (step-grid (grids 0)) [[false false]
                                  [false false]]))
    (is (= (step-grid (grids 1)) [[false false false]
                                  [true true true]
                                  [false false false]]))
    (is (= (step-grid (grids 2)) [[false false false false]
                                  [false true true false]
                                  [false true true false]
                                  [false false false false]]))))

(deftest test-pattern
  (testing "pattern->grid"
    (is (= (pattern->grid [[0 1] [1 0]])
           '([0 0 false] [1 0 true] [0 1 true] [1 1 false])))
    (is (= (pattern->grid [[0 1] [1 0]] :x 1 :y 1)
           '([1 1 false] [2 1 true] [1 2 true] [2 2 false])))
    (is (= (pattern->grid (:block patterns))
           '([0 0 false] [1 0 false] [2 0 false] [3 0 false]
             [0 1 false] [1 1 true] [2 1 true] [3 1 false]
             [0 2 false] [1 2 true] [2 2 true] [3 2 false]
             [0 3 false] [1 3 false] [2 3 false] [3 3 false]))))
  (testing "paste-pattern"
    (is (= (paste-pattern (make-grid 4 4) 0 0 (:block patterns))
           [[false false false false]
            [false true true false]
            [false true true false]
            [false false false false]]))
    (is (thrown? IndexOutOfBoundsException
                 (paste-pattern (make-grid 3 3) 0 0 (:block patterns))))
    (is (= (paste-pattern (make-grid 5 5) 0 0 (:blinker patterns))
           [[false false false false false]
            [false false true false false]
            [false false true false false]
            [false false true false false]
            [false false false false false]]))
    (is (= (-> (paste-pattern (make-grid 5 5) 0 0 (:blinker patterns))
               (step-grid))
           [[false false false false false]
            [false false false false false]
            [false true true true false]
            [false false false false false]
            [false false false false false]]))))
