(def sample1 `11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
1698522-1698528,446443-446449,38593856-38593862,565653-565659,
824824821-824824827,2121212118-2121212124`)

(defn parse-data
 [input]
 (peg/match
  ~(some
      (group
       (sequence
        (number (some :d))
        "-"
        (number (some :d))
        (between 0 1 (sequence "," (between 0 1 "\n"))))))
  input))

(defn get-invalids1
 "Для указанного диапазона возвращает список «невалидных» идентификаторов, т.е.
 тех, который состоят из двух повторяющихся наборов цифр."
 [[a b] &opt invalids]

 (default invalids @[])

 (if (> a b) invalids
  (let [r (length (string a))]
   (if (odd? r)
    # Если в числе нечётное число цифр, значит оно заведомо не может состоят из двух
    # более коротких чисел. Увеличиваем нижнюю границу диапазона до следующего разряда.
    (get-invalids [(math/pow 10 r) b] invalids)

    # Берём первую половину числа и формируем на её основе новое число. Если оно лежит
    # в границах диапазона - это подходящий «невалидный» идентификатор.
    (let [half    (-> a string (string/slice 0 (/ r 2)))
          invalid (-> half (string/repeat 2) scan-number)
          next-a  (-> half scan-number inc string (string/repeat 2) scan-number)]
     (get-invalids [next-a b] (if (<= a invalid b)
                               (array/push invalids invalid)
                               invalids)))))))

(defn get-divisors
 "Возвращает список делителей натурального числа - кроме самого числа."
 [n]
 (array/slice
  (let [top (inc (math/ceil (math/pow n 0.5)))]
   (->>
    (range 1 (inc top))
    (filter (fn [k] (zero? (% n k))))
    (map (fn [k] [k (div n k)]))
    (apply tuple/join)
    frequencies
    keys
    sorted))
  0 -2))

(defn get-invalids-for-partition
 "Для указанного диапазона и количества разбиений возвращает список «невалидных» идентификаторов, т.е.
 тех, который состоят из `n` повторяющихся наборов цифр."
 [[a b] n &opt invalids]

 (default invalids @[])

 (if (> a b) invalids
  (let [r (length (string a))]
   # Если число не делится на `n`, значит оно не может состоят из `n` более коротких
   # чисел. Увеличиваем нижнюю границу диапазона до следующего разряда.
   (if (pos? (mod r n))

    (get-invalids-for-partition [(math/pow 10 (inc r)) b] n invalids)

    # Берём первую часть числа и формируем на её основе новое число. Если оно лежит
    # в границах диапазона - это подходящий «невалидный» идентификатор.
    (let [part    (-> a string (string/slice 0 (/ r n)))
          invalid (-> part (string/repeat n) scan-number)
          next-a  (-> part scan-number inc string (string/repeat n) scan-number)]
     (get-invalids-for-partition
      [next-a b] n
      (if (<= a invalid b) (array/push invalids invalid) invalids)))))))

(get-invalids-for-partition [95 115] 2)

(get-divisors 2)

(let [parsed (parse-data (slurp "aoc25_02.txt"))]
 (reduce + 0 (tuple/join ;(map get-invalids1 parsed))))
