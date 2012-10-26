(ns ycombo)

;; tail recursion

(defn fak [x]
  (loop [n x acc 1]
    (case n
      0 acc
      (recur (- n 1) (* n acc)))))

(fak 4)

;; y-combinator

(def fac
  (fn [h, x]
    (case x
      0 1
      (* x (h h (- x 1))))))

(fac fac 4)

(def fac'
  (fn [h]
    (fn [x]
      (case x
        0 1
        (* x ((h h) (- x 1)))))))

((fac' fac') 4)

(def fac2
  (fn [h]
    (fn [x]
      (let [f (fn [q x]
                (case x
                  0 1
                  (* x (q (- x 1)))))]
        (f (h h) x)))))

((fac2 fac2) 4)

;; Pull out f

(def f
  (fn [q]
    (fn [x]
      (case x
        0 1
        (* x (q (dec x)))))))

(def fac3
  (fn [h]
    (fn [x]
      ((f (h h)) x))))

((fac3 fac3) 4)

;; Y combinator

(def Y
  (fn [f]
    (let [g (fn [h]
              (fn [x]
                ((f (h h)) x)))]
      (g g))))


(def factorial (Y (fn [recurse]
                    (fn [x]
                      (case x
                        0 1
                        (* x (recurse (dec x))))))))

(factorial 5)

;; fibonacci

(def fib
  (fn [nth]
    (case nth
      0 0
      1 1
      (+ (fib (dec nth)) (fib (dec (dec nth)))))))

(fib 7)

(def fibonacci (Y (fn [f]
                    (fn [nth]
                      (case nth
                        0 0
                        1 1
                        (+ (f (dec nth)) (f (- nth 2))))))))

(fibonacci 7)

;; f(Y(f)) = Y(f) brain asplosion

(def fac*' (Y f))
(def fac*'' (f (Y f)))

(fac*'' 5)

;; =============================================
;; fun with S K I combinators
;; =============================================

;; creates a curried version of a function
;; https://gist.github.com/745654
(defmacro def-curry-fn [name args & body]
  {:pre [(not-any? #{'&} args)]}
  (if (empty? args)
    `(defn ~name ~args ~@body)
    (let [rec-funcs (reduce (fn [l v]
                              `(letfn [(helper#
                                         ([] helper#)
                                         ([x#] (let [~v x#] ~l))
                                         ([x# & rest#] (let [~v x#]
                                                         (apply (helper# x#) rest#))))]
                                 helper#))
                            `(do ~@body) (reverse args))]
      `(defn ~name [& args#]
         (let [helper# ~rec-funcs]
           (apply helper# args#))))))

;; =============================================
;; Combinators
;; =============================================

;; S f g x = f x (g x)
(def-curry-fn S [f g x] ((f x) (g x)))

;; K x y = x
(def-curry-fn K [x y] x)

(def I identity)

;; =============================================

;; curried version of (+)
(def-curry-fn sum [x y] (+ x y))

;; "successor" reimplemented
(def suc (S (S (K sum) (K 1)) I))

;; "successor" straightforward
(def suc' (fn [x] (+ 1 x)))
