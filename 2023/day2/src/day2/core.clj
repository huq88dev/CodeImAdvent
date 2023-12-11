(ns day2.core
  (:gen-class)
  (:require [clojure.string :as str])
  )

(defn makeEntry [amount color]
  (list (symbol color) (Integer/parseInt amount))
  )

(defn swapMultiple [data]
  (if (empty? data)
  ()
  (conj(conj(swapMultiple (rest(rest data)))(first data))(second data))
  ))

(defn makeMap [& entrys]
 (apply hash-map (reduce concat entrys))
  )


(defn le [x y] 
  (if (nil? x)
    true
    (<= (Integer/parseInt x) y)
    )
  )

;configuration is a hash-map of the following scheme
;{:red int :green int :blue int} default is 0
(defn valid? [configuration]                
  (let [x configuration]
  (and                                        
    (le (get x "red") 12)
    (le (get x "green") 13)
    (le (get x "blue") 14))
  )
)

(defn preprareLine [line]
  (let [x (str/split line #": ")]
    (vector (Integer/parseInt (str/replace (first x) #"Game " ""))(str/replace (second x) #"," ""))
    )
  )

;takes a line and returns id if the line is valid. returns 0 if line is not valid
(defn evaluatePair [pair] ;pair is a Vector with [id, values]
  (let [maps (str/split (second pair) #"; ")]
    (if (reduce (fn [x y] (and x y)) 
    (map (fn [x](valid?(makeMap(swapMultiple(str/split x #" "))))) maps))
       (first pair)
      0
      )
    )
  )

(defn cubePower [hmap] 
  (* (get hmap "red")
     (get hmap "blue")
     (get hmap "green"))
  )

(defn toInt [hmap]
  (update-vals hmap (fn[x](Integer/parseInt x)))
  )

(defn findMax [& args]
    (reduce (fn [x y] (merge-with max x y))(map toInt args))
    )


(defn evaluatePair2 [pair] ;pair is a Vector with [id, values]
  (let [maps (str/split (second pair) #"; ")]
  (cubePower
   (apply findMax
        (map (fn [x](makeMap(swapMultiple(str/split x #" ")))) maps)))
   )
  )

(defn part1 [input] 
  (let [file (slurp input)]
    (reduce + (map (fn [line] (evaluatePair (preprareLine line)))
                   (str/split file #"\n")
                   ))
    )
  )

(defn part2 [input] 
  (let [file (slurp input)]
    (reduce + (map (fn [line] (evaluatePair2 (preprareLine line)))
                   (str/split file #"\n")
                   ))
    )
  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (evaluatePair2 (preprareLine "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")))
  (println (part2 "resources/input.txt"))
)


