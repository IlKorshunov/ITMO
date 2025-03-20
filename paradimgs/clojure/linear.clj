(defmacro operation [name func]
          `(defn ~name [& args#]
                 (apply mapv ~func args#)))

(operation v+ +)
(operation v- -)
(operation v* *)
(operation vd /)


(defn transpose [matrix] (apply mapv vector matrix))
(defn scalar [& vectors] (let [result (apply v* vectors)] (reduce + result)))

(defn v*s [vector scalar] (mapv #(* scalar %) vector))
(defn m*v [matrix vector] (mapv #(scalar % vector) matrix))
(defn m*s [matrix & scalars] (let [result (apply * scalars)] (mapv #( v*s % result) matrix)))
(defn m*m [matrix1 matrix2] (let [bt (transpose matrix2)] (mapv #(m*v bt %) matrix1)))
(operation m+ v+)
(operation m- v-)
(operation m* v*)
(operation md vd)

(defn skew-symmetric [vector]
      [[0        (- (vector 2)) (vector 1)]
       [(vector 2) 0         (- (vector 0))]
       [(- (vector 1)) (vector 0) 0]])

(defn vect [vector1 vector2] (m*v (skew-symmetric vector1) vector2))

(operation c+ m+)
(operation c- m-)
(operation c* m*)
(operation cd md)


(defn shape [func & args]
      (reduce #(if (and (number? %1) (number? %2))
                 (func %1 %2)
                 (mapv (partial shape func) %1 %2))
              args))

(defn s+ [a b] (shape + a b))
(defn s- [a b] (shape - a b))
(defn s* [a b] (shape * a b))
(defn sd [a b] (shape / a b))




