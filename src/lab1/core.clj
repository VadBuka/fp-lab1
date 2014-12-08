(ns lab1.core
  (:gen-class))

(def Ra 3.0)
(def Rb (* 1.5 Ra))
(def Emax 0.6)
(def Emin 0.2)

(def pot_a_koeff (/ 4 (Math/pow Ra 2)))
(def pot_b_koeff (/ 4 (Math/pow Rb 2)))

(declare revise-potentials)
(declare max-potential-point)
(declare shortest-distance)

(defn distance-by-euclid [coords1 coords2]
  {:pre [(= (count coords1) (count coords2))]}
  (Math/sqrt (apply + (map #(Math/pow (- %1 %2) 2) coords1 coords2))))

(defn distance-by-hamming [coords1 coords2]
  {:pre [(= (count coords1) (count coords2))]}
  (apply + (map #(if (= %1 %2) 0 1) coords1 coords2)))

(defn reset-max-point-potential [points target-point]
  (map
    (fn [point]
      (if
        (=
          (point :coords)
          (target-point :coords))
        (assoc point :potential 0.0)
        point))
    points))

(defn find-centers
  ([points distance-function]
    (find-centers points nil [] distance-function))

  ([points first-center-potential centers distance-function]
    (let [max-point (max-potential-point points)
         revised-points (revise-potentials points max-point distance-function)]

      (if-not first-center-potential
        (recur revised-points (max-point :potential) [max-point] distance-function)
        (if
          (>
            (max-point :potential)
            (* Emax first-center-potential))
          (recur revised-points first-center-potential (conj centers max-point) distance-function)
          (if
            (<
              (max-point :potential)
              (* Emin first-center-potential))
            centers
            (if
              (>=
                (+
                  (/ (shortest-distance max-point centers distance-function) Ra)
                  (/ (max-point :potential) first-center-potential))
                1)
              (recur revised-points first-center-potential (conj centers max-point) distance-function)
              (recur
                (reset-max-point-potential revised-points max-point)
                first-center-potential
                centers
                distance-function))))))))

(defn load-points-from-file [path]
  {:pre [(not (nil? path))]}
  (with-open [r (clojure.java.io/reader path)]
    (doall (map (fn [line]
      {:coords (vec (map #(Double/parseDouble %) (drop-last (.split #"," line))))
        :potential 0.0})
      (line-seq r)))))

(defn calc-potential [point, points, distance-function]
  (let [new-potential
        (reduce
          (fn [res other-point]
          (+
            res
            (Math/pow
              Math/E
              (*
                (- pot_a_koeff)
                (distance-function
                  (point :coords)
                  (other-point :coords))))))
            0.0
            points)]

    (assoc point :potential new-potential)))

(defn calc-potentials [points distance-function]
  (map #(calc-potential % points distance-function) points))

(defn revise-potential [point points base-point distance-function]
  (let [revised-potential
        (-
          (point :potential)
          (*
            (base-point :potential)
            (Math/pow
              Math/E
              (*
                (- pot_b_koeff)
                (distance-function
                  (point :coords)
                  (base-point :coords))))))]

    (assoc point :potential revised-potential)))

(defn revise-potentials [points base-point distance-function]
  (map #(revise-potential % points base-point distance-function) points))

(defn max-potential-point [points]
  (apply max-key
    (fn [point] (point :potential))
    points))

(defn shortest-distance [point points distance-function]
  (Math/sqrt (apply min (map #(distance-function (point :coords) (% :coords)) points))))

(defn -main
  [distance-method file]
  (let [distance-function (case distance-method
                        "euclid" distance-by-euclid
                        "hamming" distance-by-hamming)
        points (load-points-from-file file)
        points (calc-potentials points distance-function)]
    (doseq [center (find-centers points distance-function)] (println center))))
