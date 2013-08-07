(ns ski)

;; =============================================
;; fun with S K I combinators
;; =============================================

;; creates a curried version of a function
;; https://gist.github.com/sunilnandihalli/745654
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
;; S K I Combinators
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
