;; Function to create all possible selections
(defn conjall [lst vec]
    (cond (empty? lst) '()
        :else
          (cons (conj  vec  (first lst))
		     (conjall (rest lst) vec))))
             
(defn selections [n m]
  (cond 
     (= n 0) '([])
      :else
          (apply concat (map (fn [it] (conjall (range 1 (inc m)) it)) 
                                                               (selections (dec n) m)))))


;; Generating all possible rotations
 (defn rotate-str [s]
(vec(concat (rest s) [(first s)])))

(defn gen-rotations [item]
(vec(concat (take (count item) (iterate rotate-str item)) )))
 


;; generating reflections keeping the two middle beads fixed(only considered one case)
(defn group [chars]
  (cond (empty? chars) chars
	(empty? (next chars)) (list chars)
	:else
	(let [dres (group (next chars))]
	  (cond (combining? (second chars)) (cons (cons (first chars)
							(first dres))
						  (rest dres))
		:else (cons (list (first chars)) dres)))))
 
(defn str-reverse  [s]
  (vec (apply concat (reverse (group s)))))

(defn isN? [a]
  (cond
    (= a (first(sort (gen-rotations a))))
     (< 1 2)
    :else
      (< 2 1)
   ))
(defn isB? [a]
  (cond
    (= a (first(sort (gen-rotations(str-reverse a)))))
     (< 1 2)
    :else
      (< 2 1)
   ))


;;Generating the necklaces
(defn necklace [a]
  (for [ x a
:when (isN? x)]
x))

;;Generating the bracelets
 (defn bracelets [a]
  (for [ x a
:when (isB? x)]
x))

;; Implementation (NECKLACES)
(necklace (selections 7 3))

;;Implementation (BRACELETS)
(bracelet (selections 7 3))


