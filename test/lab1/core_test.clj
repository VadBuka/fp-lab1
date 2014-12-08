(ns lab1.core-test
  (:require [clojure.test :refer :all]
            [lab1.core :refer :all]))

(deftest test-distance-by-euclid
  (is (=
    (distance-by-euclid [10 27] [5 15])
    13.0))

  (is (thrown? AssertionError (distance-by-euclid [5 2] [6 4 28]))))

(deftest test-calc-potential
  (let [point1 {:coords [0.0 3.0], :potential 0.0}
        point2 {:coords [1.0 5.0], :potential 0.0}
        distance-function distance-by-euclid]

    (is (=
      (-> (calc-potential point2 [point1 point2] distance-function) :potential)
      1.3701644213593966))))

(deftest test-distance-by-hamming
  (is (=
    (distance-by-hamming [2 1 7 3 8 9 6] [2 2 3 3 7 9 6])
    3))

  (is (thrown? AssertionError (distance-by-hamming [5 2] [6 4 28]))))

(deftest test-reset-max-point-potential
  (is (=
    (reset-max-point-potential
      [{:coords [5.0 8.0], :potential 3.0} {:coords [6.0 1.0], :potential 5.0}]
      {:coords [6.0 1.0], :potential 5.0})
    [{:coords [5.0 8.0], :potential 3.0} {:coords [6.0 1.0], :potential 0.0}])))

(deftest test-load-points-from-file
  (let [path "test/fixtures/points.txt"
        points [{:coords [5.0 8.0], :potential 0.0} {:coords [6.0 1.0], :potential 0.0}]]
    (is (=
      (load-points-from-file path)
      points)))

  (is (thrown? AssertionError (load-points-from-file nil)))

  (is (thrown?
    java.io.FileNotFoundException
    (load-points-from-file "non-existing-file.txt"))))
