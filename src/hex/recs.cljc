(ns hex.recs
  "We're using defrecord to communicate with the EDN between server/client.
  Like a.
  '''
  (edn/read-string {:readers {'chsv.recs.Cell map->Cell}} \"#chsv.recs.Cell{:cx 1 :cy 2}\")
  '''
  Mainly using Cube records to calculate.
  Axis and Cell records are used for expression system.

  +y -z  +x
  　＼｜／
  　　△　　　　　　　　Cube
  　／｜＼
  -x +z -y　　    　　　
  |-----|-----|-----|-----|-----|---
  |-1,-2| 0,-2| 1,-2| 2,-2| 3,-2|
  |     |     |     |     |     |
  |  3  |  2  |  1  |  0  | -1  |
  ---|-----|-----|-----|-----|-----|
     |-1,-1| 0,-1| 1,-1| 2,-1| 3,-1|
     |     |     |     |     |     |
     |  2  |  1  |  0  | -1  | -2  |
  |-----|-----|-----|-----|-----|---
  |-2,0 |-1,0 | x,z | 1,0 | 2,0 |
  |     |     |     |     |     |
  | +2  | +1  |  y  | -1  | -2  |
  ---|-----|-----|-----|-----|-----|
     |-2,1 |-1,1 | 0,1 | 1,1 | 2,1 |
     |     |     |     |     |     |
     | +1  |  0  | -1  | -2  | -3  |
  |-----|-----|-----|-----|-----|---
  |-3,2 |-2,2 |-1,2 | 0,2 | 1,2 |
  |     |     |     |     |     |
  |  1  |  0  | -1  | -2  | -3  |
  ---|-----|-----|-----|-----|-----|

  * Axis record coordinates.
  -q
   ＼
  ---*--- +r
      ＼
  |-----|-----|-----|-----|-----|---
  |     | 0,-2| 1,-2|     |     |
  |     |     |     |     |     |
  |     |     |     |     |     |
  ---|-----|-----|-----|-----|-----|
     |-1,-1| 0,-1| 1,-1|     |     |
     |     |     |     |     |     |
     |     |     |     |     |     |
  |-----|-----|-----|-----|-----|---
  |-2,0 |-1,0 | q,r | 1,0 | 2,0 |
  |     |     |     |     |     |
  |     |     |     |     |     |
  ---|-----|-----|-----|-----|-----|
     |     |-1,1 | 0,1 |     |     |
     |     |     |     |     |     |
     |     |     |     |     |     |
  |-----|-----|-----|-----|-----|---
  |     |     |     | 0,2 |     |
  |     |     |     |     |     |
  |     |     |     |     |     |
  ---|-----|-----|-----|-----|-----|
  * Cell coordinates as offset.
                cy (indicated dot cx = 0)
                 ↓
  |-----|-----|-----|-----|-----|---
  |     |     |  .  |     |     |
  |-2,-2|-1,-2| 0,-2| 1,-2| 2,-2|
  |     |     |     |     |     |
  ---|-----|-----|-----|-----|-----|
     |     |     |  .  |     |     |
     |     |-1,-1| 0,-1|     |     |
     |     |     |     |     |     |
  |-----|-----|-----|-----|-----|---
  |     |     |  .  |     |     |
  |-2,0 |-1,0 |cx,cy| 1,0 |2,0  |   ->cx
  |     |     |     |     |     |
  ---|-----|-----|-----|-----|-----|
     |     |     |  .  |     |     |
     |     |-1,1 | 0,1 | 1,1 | 2,1 |
     |     |     |     |     |     |
   -----|-----|-----|-----|-----|---
  |     |     |  .  |     |     |
  |     |-1,2 | 0,2 | 1,2 |     |
  |     |     |     |     |     |
  ---|-----|-----|-----|-----|---

  * Neigbour d is direction.
  |-----|-----|-----|-----|-----|---
  |     |     |     |     |     |
  |     |     |     |     |     |
  |     |     |     |     |     |
  ---|-----|-----|-----|-----|-----|
     |     |     |     |     |     |
     |     | d:2 | d:1 |     |     |
     |     |     |     |     |     |
  |-----|-----|-----|-----|-----|---
  |     |     |     |     |     |
  |     | d:3 |  O  | d:0 |     |
  |     |     |     |     |     |
  ---|-----|-----|-----|-----|-----|
     |     |     |     |     |     |
     |     | d:4 | d:5 |     |     |
     |     |     |     |     |     |
   -----|-----|-----|-----|-----|---
  |     |     |     |     |     |
  |     |     |     |     |     |
  |     |     |     |     |     |
  ---|-----|-----|-----|-----|---"
  #?(:clj (:refer-clojure :exclude [peek pop!]))
  (:require [hex.priority-set :as ps]))

(defprotocol IDistance
  "Return hexagonal distance, Manhattan distance"
  (distance [_ to]))

(defprotocol IMath
  (add [this p2])
  (sub [this p2])
  (scale [this scale] "scalar"))

(defprotocol IAccessor
  (get-x [_] "point-x")
  (get-y [_] "point-y")
  (get-z [_] "point-z"))

(defprotocol ISquaredDistance
  "distance without sqrt, Euclidean distance"
  (distance-squared [this to]))

(defrecord Point [x y]
  IAccessor
  (get-x [_] x)
  (get-y [_] y)

  IMath
  (add [this p2] (Point. (+ x (:x p2)) (+ y (:y p2))))
  (sub [this p2] (Point. (- x (:x p2)) (- y (:y p2))))
  (scale [this scale] (Point. (* x scale) (* y scale)))

  ISquaredDistance
  (distance-squared [this to] (let [dx (- x (:x to))
                                    dy (- y (:y to))]
                                (+ (* dx dx) (* dy dy)))))

(defprotocol INeighborhood
  (neighbor [_ direction] "The direction to obtain the offset to the next hexa is from 0 to 5, with 0 pointing to the right of the x-axis. It is counterclockwise.")
  (ring [_ radious] "Radius Based Neighborhood")
  (spiral [_ radious] "Radius-based filled-in neighbors"))

(defprotocol IAccessorOffset
  (get-cx [_])
  (get-cy [_]))

(def cell-direction-odd
  [[1  0]
   [0  1]
   [-1  1]
   [-1  0]
   [-1 -1]
   [0 -1]])

(def cell-direction-even
  [[1  0]
   [1  1]
   [0  1]
   [-1  0]
   [0 -1]
   [1 -1]])

(defrecord Cell [cx cy]
  IAccessorOffset
  (get-cx [_] cx)
  (get-cy [_] cy))

(def rad-30 (* 30 (/ Math/PI 180)))
(def rad-60 (* 60 (/ Math/PI 180)))

(defn- vertex-num-to-rad
  [i]
  (- (* rad-60 i) rad-30)
  #_(* rad-60 i))

(defn- vertex-num-to-x-pos
  [i])

(def design-of-hex-point
  "Generate a hexa inscribed in a circle of radius 1, which
  Later, you can zoom in on the object to be displayed as a target in the app"
  (mapv (fn [i] (Point. (* (Math/cos (vertex-num-to-rad i)))
                        (* (Math/sin (vertex-num-to-rad i))))) (range 6)))

(defprotocol ICubeToAxis
  "coordinate system transformation"
  (cube->axis [_]))

(defprotocol ICubeToCell
  "coordinate system transformation"
  (cube->cell [_]))

(defprotocol ICubeMath
  "Cube arithmetic"
  (round [_])
  (line-to [_ b] "The return value is an vec of the Cube."))

(defprotocol IAccsessorAxis
  (get-r [_])
  (get-q [_]))

(defrecord Axis [q r]
  IAccsessorAxis
  (get-q [_] q)
  (get-r [_] r))

(declare cube-dir-offset
         cube-lerp)

(defrecord Cube [x y z]
  IAccessor
  (get-x [_] x)
  (get-y [_] y)
  (get-z [_] z)

  IDistance
  (distance [_ to]
    (max (Math/abs (- x (get-x to)))
         (Math/abs (- y (get-y to)))
         (Math/abs (- z (get-z to)))))

  IMath
  (add [_ p2] (Cube. (+ x (get-x p2)) (+ y (get-y p2)) (+ z (get-z p2))))
  (sub [_ p2] (Cube. (- x (get-x p2)) (- y (get-y p2)) (- z (get-z p2))))
  (scale [_ scale] (Cube. (* x scale) (* y scale) (* z scale)))

  ICubeToAxis
  (cube->axis
    [_]
    (let [q x
          r z]
      (Axis. q r)))

  ICubeToCell
  (cube->cell
    [_]
    (let [col (+ x
                 (/ (- z (bit-and z 1)) 2))
          row z]
      (Cell. col row)))

  INeighborhood
  (neighbor
    [this direction] (add this (nth cube-dir-offset direction)))

  (ring [this radious]
    (loop [result []
           i      0
           j      0
           cube   (add this (scale (nth cube-dir-offset 4) radious))]
      (if (< i 6)
        (if (< j radious)
          (recur (conj result cube)
                 i
                 (inc j)
                 (neighbor cube i))
          (recur result (inc i) 0 cube))
        result)))

  (spiral [this radious]
    (loop [result [this]
           k      1]
      (if (<= k radious)
        (recur (into result (ring this k))
               (inc k))
        result)))

  ICubeMath
  (round [_]
    (let [rx     (Math/round x)
          ry     (Math/round y)
          rz     (Math/round z)
          diff-x (Math/abs (- rx x))
          diff-y (Math/abs (- ry y))
          diff-z (Math/abs (- rz z))]
      (if (and (> diff-x diff-y) (> diff-x diff-z))
        (Cube. (- (- ry) rz), ry, rz)
        (if (> diff-y diff-z)
          (Cube. rx (- (- rx) rz) rz)
          (Cube. rx ry (- (- rx) ry))))))

  (line-to [this b]
    (let [n (distance this b)]
      (when (not (zero? n))
        (let [d (/ 1.0 n)]
          (mapv (fn [i] (round (cube-lerp this b (* i d))))
                (range (inc n))))))))

(def cube-dir-offset
  [(Cube. 1 -1 0) (Cube. 1 0 -1) (Cube. 0 1 -1)
   (Cube. -1 1 0) (Cube. -1 0 1) (Cube. 0 -1 1)])

(defn- lerp
  [a b t]
  (+ a (* (- b a) t)))

(defn cube-lerp
  "must apply round function"
  [a b t]
  (Cube. (lerp (get-x a) (get-x b) t)
         (lerp (get-y a) (get-y b) t)
         (lerp (get-z a) (get-z b) t)))

(defn cell->cube
  [cell]
  (let [x (- (:cx cell) (/ (- (:cy cell) (bit-and (:cy cell) 1)) 2))
        z (:cy cell)
        y (- (- x) z)]
    (Cube. x y z)))

(defn axis->cube
  ([axis]
   (let [x (get-q axis)
         z (get-r axis)
         y (- (- x) z)]
     (Cube. x y z)))
  ([q r]
   (let [x q
         z r
         y (- (- x) z)]
     (Cube. x y z))))

(def hex-width-coefficient
  (Math/cos (/ Math/PI 6)))

(defn cell->point
  "Generate a Point from a Cell. size is the radius of the circumscribed circle of Hex"
  [cell size]
  (let [width  (* size hex-width-coefficient 2)
        shift  (if (odd? (:cy cell))
                 (* size hex-width-coefficient)
                 0)
        ;; 正六角形を構成する正三角形の一辺の長さ分詰める(.75)
        height (* size 2 0.75)]
    (Point. (+ shift (* (:cx cell) width))
            (+ (* (:cy cell) height)))))

(def sqrt3 (Math/sqrt 3.0))
(def sqrt3div3 (/ sqrt3 3.0))
(def sqrt3div2 (/ sqrt3 2.0))
(def const1div3 (/ 1. 3))
(def const2div3 (/ 2. 3))
(def const3div2 (/ 3. 2))

(defn point->axis
  "Returns the coordinates of the Hex pointed by point.
  px py is the coordinate of a pixel, size is the circumscribed circle of a hexa"
  ([px py size]
   (let [q (/ (- (* sqrt3div3 px) (* const1div3 py)) size)
         r (/ (* const2div3 py) size)]
     (cube->axis (round (axis->cube q r)))))
  ([^Point point size]
   (let [q (/ (- (* sqrt3div3 (get-x point)) (* const1div3 (get-y point))) size)
         r (/ (* const2div3 (get-y point)) size)]
     (cube->axis (round (axis->cube q r))))))

(defn point->cell
  "Returns the coordinates of the Hex pointed by point.
  px py is the coordinate of a pixel, size is the circumscribed circle of a hexa."
  ([px py size]
   (let [q (/ (- (* sqrt3div3 px) (* const1div3 py)) size)
         r (/ (* const2div3 py) size)]
     (cube->cell (round (axis->cube q r)))))
  ([^Point point size]
   (let [q (/ (- (* sqrt3div3 (get-x point)) (* const1div3 (get-y point))) size)
         r (/ (* const2div3 (get-y point)) size)]
     (cube->cell (round (axis->cube q r))))))

(defn axis->point
  "Generate a Point from a Axis. size is the radius of the circumscribed circle of Hex"
  [axis size]
  (let [x (* size (+ (* (get-q axis) sqrt3) (* sqrt3div2 (get-r axis))))
        y (* size const3div2 (get-r axis))]
    (Point. x y)))

(defn path-finding
  "Finds a path and returns a map with the goal as key and the adjacent destination as val.
  ```
  (loop [result () g goal p this-result]
    (if-let [chain (p g)]
      (recur (cons g result) chain p)
      (cons g result)))
  ```
  This is the list from the start to goal, with code like this"
  ([^Cube start ^Cube goal obstacle-set graph-cost-map]
   (path-finding ^Cube start ^Cube goal obstacle-set graph-cost-map 9999))
  ([^Cube start ^Cube goal obstacle-set graph-cost-map maximum-frontier-length]
   (let [frontier (ps/priority-set 0 start)]
     (loop [came-from   {start nil}
            cost-so-far {start 0}
            current     (ps/pop! frontier)
            neighbor    (when current (ring current 1))
            next-zoc    (first neighbor)]
       (if (and current (< (count frontier) maximum-frontier-length))
         (if (= current goal)
           came-from
           (if-not next-zoc
             (let [current  (ps/pop! frontier)
                   neighbor (when current (ring current 1))
                   next-zoc (first neighbor)]
               (recur came-from
                      cost-so-far
                      current
                      neighbor
                      next-zoc))
             (let [new-cost      (+ (cost-so-far current) (get graph-cost-map next-zoc 1))
                   prio          (+ new-cost (distance goal next-zoc)) ;; prioはdistanceでなくhuristicである(本義)
                   rest-neigbour (rest neighbor)]
               (if (and (not (obstacle-set next-zoc)) (or (not (cost-so-far next-zoc)) (< new-cost (cost-so-far next-zoc))))
                 (do
                   (ps/push! frontier prio next-zoc)
                   (recur (assoc came-from next-zoc current)
                          (assoc cost-so-far next-zoc new-cost)
                          current
                          rest-neigbour
                          (first rest-neigbour)))
                 (recur came-from
                        cost-so-far
                        current
                        rest-neigbour
                        (first rest-neigbour)))))))))))

(defn path-graph->cubevec
  "convert path-finding result to simple list"
  [path-graph goal-cube]
  (loop [result () g goal-cube p path-graph]
    (if-let [chain (p g)]
      (recur (cons g result) chain p)
      (cons g result))))


(def edn-reader-map {'hex.recs.Point map->Point
                     'hex.recs.Cell map->Cell
                     'hex.recs.Axis map->Axis
                     'hex.recs.Cube map->Cube})



