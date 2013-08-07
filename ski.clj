(ns ski
  (:use [curry :only (def-curry-fn)]))

;; =============================================
;; fun with S K I combinators
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
