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