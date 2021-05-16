(ns astar.core
  (:require
    [clojure.string :as S :refer [join]]
    [clojure.pprint :as PP :refer [pprint]]))

(defn debug [& x]
  (println x)
  x)

(defn dedupe-by 
  ([f coll] 
    (first (reduce (partial dedupe-by f) [[] nil] coll)))
  ([f [acc pv] item]
    (let [v (f item)
          nacc (conj acc item)]
    (cond (= pv v) [acc pv]
          :else    [nacc v]))))

(defn index-of [x coll]
  (let [idx? (fn [i a] (when (= x a) i))]
  (first (keep-indexed idx? coll))))

(defn filter-out [f coll]
  (filter (complement f) coll))

(defn manhattan [[x1 y1] [x2 y2]]
  (+ (Math/abs ^Integer (- x2 x1)) 
     (Math/abs ^Integer (- y2 y1))))

(defn costs [src dest curr]
  (let [g (manhattan src curr)
        h (manhattan curr dest)
        f (+ g h)]
  [f g h]))

(defn trace-path
  ([dest clsd]
    (trace-path [] dest clsd))
  ([path dest clsd]
    (let [npath (cons dest path)
          ndest (clsd dest)]
    (cond (nil? ndest) npath
          :else        (recur npath ndest clsd)))))

(defn neibhors [rows cols [x y]]
  (for [a (range (dec x) (+ 2 x)) :when (and (>= a 0) (<= a rows))
        b (range (dec y) (+ 2 y)) :when (and (>= b 0) (<= b cols))
        :when (not= [a b] [x y])]
    [a b]))

(defn neibhor-nodes [curr [neigh [f g h]]]
  {:c neigh, :f f, :g g, :h h, :p curr})

(defn closed? [clsd node] 
  (contains? clsd node))

(defn blocked? [grid [x y]] 
  (= 1 (nth (nth grid x) y)))

(defn astar
  ([grid src dest]
    (let [rows (dec (count grid))
          cols (dec (count (first grid)))
          open [{:c src, :f 0, :g 0, :h 0, :p nil}]
          clsd {}]
    (astar grid rows cols src dest open clsd)))
  ([grid rows cols src dest open clsd]
      (let [[curr & res] (sort-by :f open)
            {:keys [c p]}  curr
            nclsd    (assoc clsd c p)
            closed?  (partial closed? clsd)
            blocked? (partial blocked? grid)
            costs    (partial costs src dest)]
      (cond (nil? curr) nil
            (= dest c)  (trace-path c nclsd)
            (closed? c) (recur grid rows cols src dest res clsd)
            :else       (let [neigh  identity
                              neighs (->> (neibhors rows cols c)
                                          (filter-out (some-fn closed? blocked?))
                                          (map (juxt neigh costs))
                                          (map (partial neibhor-nodes c)))
                              nopen  (->> (concat res neighs)
                                          (sort-by (juxt :c :g))
                                          (dedupe-by :c))]
                        (recur grid rows cols src dest nopen nclsd))))))

(defn astar-traces
  ([grid src dest]
    (let [rows (dec (count grid))
          cols (dec (count (first grid)))
          open [{:c src, :f 0, :g 0, :h 0, :p nil}]
          clsd {}]
    (astar-traces grid rows cols src dest open clsd)))
  ([grid rows cols src dest open clsd]
      (let [[curr & res] (sort-by :f open)
            {:keys [c p]}  curr
            nclsd    (assoc clsd c p)
            closed?  (partial closed? clsd)
            blocked? (partial blocked? grid)
            costs    (partial costs src dest)]
      (cond (nil? curr) nil
            (= dest c)  (list (trace-path c nclsd))
            (closed? c) (lazy-seq (cons (trace-path c clsd)
                                        (astar-traces grid rows cols src dest res clsd)))
            :else       (let [neigh  identity
                              neighs (->> (neibhors rows cols c)
                                          (filter-out (some-fn closed? blocked?))
                                          (map (juxt neigh costs))
                                          (map (partial neibhor-nodes c)))
                              nopen  (->> (concat res neighs)
                                          (sort-by (juxt :c :g))
                                          (dedupe-by :c))]
                        (lazy-seq (cons (trace-path c nclsd)
                                        (astar-traces grid rows cols src dest nopen nclsd))))))))

(defn print-grid [grid]
  (doseq [row grid]
    (println (str "|" (S/join "|" row) "|")))
  (println))

(defn draw-grid
  ([grid src dest path]
    (let [mapr (fn [r row] (map-indexed (partial draw-grid grid path src dest r) row))]
    (map-indexed mapr grid)))
  ([grid path src dest r c v]
    (cond (= [r c] src)         "S"
          (= [r c] dest)        "E"
          (index-of [r c] path) "*"
          (= v 1)               "#"
          :else                 " ")))

(def visualize (comp print-grid draw-grid))

(def grid [[0 0 0 1 0]
           [0 1 0 1 0]
           [0 1 0 1 0]
           [0 1 0 1 0]
           [0 1 0 0 0]])

(def src  [4 0])
(def dest [0 4])

(visualize grid src dest (astar grid [4 0] [0 4]))

(dorun (map (partial visualize grid src dest) (astar-traces grid [4 0] [0 4])))
