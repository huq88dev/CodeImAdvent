(ns day1.core
  (:gen-class) 
    )

(require '[clojure.string :as str])

(def numbers "0123456789")

(def mapping {"zero""zer0o" "one" "o1ne" "two" "tw2o" "three" "thre3e" "four" "fou4r" 
             "five" "fiv5e" "six" "si6x" "seven" "seve7n" "eight" "eigh8t" "nine" "nin9e"
              })


(defn firstNumber [line]
               (reduce
                 (fn [x y]   
                            (if (str/includes? numbers x) 
                              x 
                              y
                              ))
               (str/split line #""))
  )             


  (defn lastNumber [line]
               (reduce
                 (fn [x y]   
                            (if (str/includes? numbers y) 
                              y
                              x
                              ))
               (str/split line #""))
  )

(defn firstAndLast [line] 
  (Integer/parseInt(str (firstNumber line) (lastNumber line)))
  )

(defn part1 [input]
  (let [file (slurp input)]
    (reduce 
      + 
      (map 
          firstAndLast
          (str/split file #"\n")))))



;Part2

(defn resolveWords [line]
  (str/replace line #"zero|one|two|three|four|five|six|seven|eight|nine" mapping)
  )

(defn part2 [input]
  (let [file (slurp input)]
    (reduce 
      + 
      (map 
          firstAndLast
          (map 
            (fn [x] (resolveWords(resolveWords x)))
            (str/split file #"\n")))))
            )
  

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (resolveWords "twonenine"))
  (println (part2 "resources/input.txt" )))
