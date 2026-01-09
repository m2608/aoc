(def sample1 `L68
L30
R48
L5
R60
L55
L1
L99
R14
L82`)

(def start 50)
(def total 100)

(defn parse-data
 [input]
 (peg/match ~(any (sequence
                   (replace
                    (sequence
                     (<- (choice "L" "R"))
                     (number (some :d)))
                    ,(fn [dir value] (* (if (= dir "L") -1 1) value)))
                   (between 0 1 "\n")))
  input))

(comment (parse-data sample1)) # @[-68 -30 48 -5 60 -55 -1 -99 14 -82]

(defn count-zeroes1
 "Увеличивает аккумулятор, если при повороте ручка остановилась на нуле."
 [[acc zeroes] v]
 (let [new-acc (+ acc v)]
  [new-acc
  ((if (zero? (mod new-acc total)) inc identity) zeroes)]))

(defn part1
 [parsed-input]
 (->
  (reduce count-zeroes1 [start 0] parsed-input)
  (get 1)))

(defn count-zeroes2
 "Увеличивает аккумулятор на количество пройденных нулей при повороте ручки."
 [[acc zeroes] v]
 (let [new-acc (mod (+ acc v) total)
       new-zeroes (+ (div (math/abs v) total)
                     (if (or (zero? acc) (< 0 (+ acc (% v total)) total)) 0 1))]
  # (print acc " " new-acc " " new-zeroes)
  [new-acc
   (+ zeroes new-zeroes)]))

(defn part2
 [parsed-input]
 (->
  (reduce count-zeroes2 [start 0] parsed-input)
  (get 1)))

(let [parsed (parse-data (slurp "aoc25_01.txt"))]
 (print (part1 parsed))
 (print (part2 parsed)))
