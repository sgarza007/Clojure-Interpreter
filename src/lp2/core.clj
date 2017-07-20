(ns lp2.core (:gen-class)
(:require [clojure.data.json :as json]))

;;ExprC Types
(deftype numC [n])
(deftype idC [s])
(deftype trueC [])
(deftype falseC [])
(deftype plusC [l r])
(deftype multC [l r])
(deftype ifC [c tE fE])
(deftype equalC [e1 e2])
(deftype lamC [param body])
(deftype letC [s val body])
(deftype absC [e])
(deftype pairC [fst snd])
(deftype fstC [p])
(deftype sndC [p])
(deftype letrecC [id rhs body])
(deftype appC [fun arg])
(deftype consC [first rest])
(deftype emptyC? [l])
(deftype firstC [l])
(deftype restC [l])
(deftype emptyC [l])

;Function that gets the binding
(defn lookup [id env]
  (get env id))

;;Value Types
(deftype numV [n])
(deftype boolV [b])
(deftype closV [n body env])
(deftype pairV [fst snd])
(deftype listV [l])

(defn parse [expr]
  (case (get expr :expr)
    "number" (numC. (get expr :number))

    "boolean" (if (get expr :bool)
                  (trueC.) (falseC.))

    "id" (idC. (get expr :id))

    "add" (plusC. (parse (get expr :left))
                  (parse (get expr :right)))

    "mult" (multC. (parse (get expr :left))
                    (parse (get expr :right)))

    "if" (ifC. (parse (get expr :cond))
          (parse (get expr :then))
          (parse (get expr :else)))

    "equal" (equalC. (parse (get expr :left))
                      (parse (get expr :right)))

    "let" (letC. (get expr :id)
                      (parse (get expr :val))
                      (parse (get expr :body)))

    "lambda" (lamC. (get expr :param)
                    (parse (get expr :body)))

    "first" (firstC. (parse (get expr :list)))

    "rest" (restC. (parse (get expr :list)))

    "cons" (consC. (parse (get expr :first))
                    (parse (get expr :rest)))

    "empty" (emptyC. ())

    "empty?" (emptyC?. (parse (get expr :list)))

    "application" (appC. (parse (get expr :func))
                          (parse (get expr :arg))))
)





;This is our Environment
;;The env is a hashmap
(deftype interp-inputs [expr env])


;;Interp which takes in a interp-intputs
;we take the expr part of the interp-inputs to match up the correct method
(defmulti interp (fn [x] (class (.expr x))))

(defmethod interp numC [expr]
  (numV. (.n (.expr expr))))

;sends the symbol and the current env into our lookup function
(defmethod interp idC [expr]
  (lookup (.s (.expr expr)) (.env expr)))

(defmethod interp plusC [expr]
  (if (and (instance? numV (interp (interp-inputs. (.l (.expr expr)) (.env expr))))
            (instance? numV (interp (interp-inputs. (.r (.expr expr)) (.env expr)))))

            (numV. (+ (.n (interp (interp-inputs. (.l (.expr expr)) (.env expr))))
            (.n (interp (interp-inputs. (.r (.expr expr)) (.env expr))))))

            ((println "error. Not a number")
            (System/exit 0))))

(defmethod interp multC [expr]
  (if (and (instance? numV (interp (interp-inputs. (.l (.expr expr)) (.env expr))))
            (instance? numV (interp (interp-inputs. (.r (.expr expr)) (.env expr)))))

            (numV. (* (.n (interp (interp-inputs. (.l (.expr expr)) (.env expr))))
            (.n (interp (interp-inputs. (.r (.expr expr)) (.env expr))))))

            ((println "error. Not a number")
            (System/exit 0))))

(defmethod interp trueC [expr]
  (boolV. true))

(defmethod interp falseC [expr]
  (boolV. false))

(defmethod interp ifC [expr]
  (if (instance? boolV (interp (interp-inputs. (.c (.expr expr)) (.env expr))))

  (if (.b (interp (interp-inputs. (.c (.expr expr)) (.env expr))))
    (interp (interp-inputs. (.tE (.expr expr)) (.env expr)))
    (interp (interp-inputs. (.fE (.expr expr)) (.env expr))))

    ((println "error. Not a number")
    (System/exit 0))))


(defmethod interp equalC [expr]
  (if (and (instance? numV (interp (interp-inputs. (.e1 (.expr expr)) (.env expr))))
            (instance? numV (interp (interp-inputs. (.e2 (.expr expr)) (.env expr)))))

  (if (= (.n (interp (interp-inputs. (.e1 (.expr expr)) (.env expr))))
         (.n (interp (interp-inputs. (.e2 (.expr expr)) (.env expr)))))
    (boolV. true)
    (boolV. false))

    ((println "error. Can't compare two booleans")
    (System/exit 0))))

(defmethod interp absC [expr]
  (if (instance? numV (interp (interp-inputs. (.e (.expr expr)) (.env expr))))

  (if (neg? (.n (interp (interp-inputs. (.e (.expr expr)) (.env expr)))))
    (numV. (* -1 (.n (interp (interp-inputs. (.e (.expr expr)) (.env expr))))))
    (interp (interp-inputs. (.e (.expr expr)) (.env expr))))

    ((println "error. Can't take the absolute value of a non-number")
    (System/exit 0))))

(defmethod interp lamC [expr]
  (closV. (.param (.expr expr)) (.body (.expr expr)) (.env expr)))


(defmethod interp letC [expr]
  (interp (interp-inputs. (.body (.expr expr))
                          (assoc (.env expr) (.s (.expr expr))
                          (interp (interp-inputs. (.val (.expr expr)) (.env expr)))))))

(defmethod interp emptyC? [expr]
  (if (empty? (.l (interp (interp-inputs. (.l (.expr expr)) (.env expr)))))
    (boolV. true)
    (boolV. false)))

(defmethod interp emptyC [expr]
  (listV. '()))

  (defmethod interp firstC [expr]
    (if (instance? listV (interp (interp-inputs. (.l (.expr expr)) (.env expr))))

    (listV. (cons (first (.l (interp (interp-inputs. (.l (.expr expr)) (.env expr))))) '()))

    ((println "error. Cannot get first of something to a non-list")
    (System/exit 0))))

(defmethod interp consC [expr]
  (if (instance? listV (interp (interp-inputs. (.rest (.expr expr)) (.env expr))))

  (listV. (cons (interp (interp-inputs. (.first (.expr expr)) (.env expr)))
          (.l (interp (interp-inputs. (.rest (.expr expr)) (.env expr))))))

          ((println "error. Cannot cons something to a non-list")
          (System/exit 0))))

(defmethod interp restC [expr]
  (if (instance? listV (interp (interp-inputs. (.l (.expr expr)) (.env expr))))

  (listV. (rest (.l (interp (interp-inputs. (.l (.expr expr)) (.env expr))))))

  ((println "error. Cannot get rest something to a non-list")
  (System/exit 0))))

(defmethod interp appC [expr]
  (if (instance? closV (interp (interp-inputs. (.fun (.expr expr)) (.env expr))))

  (let [bind (interp (interp-inputs. (.arg (.expr expr)) (.env expr)))]
    (interp (interp-inputs. (.body (.fun (.expr expr)))
                            (assoc (.env expr) (.param (.fun (.expr expr))) bind))))

      ((println "error. Not applying function")
      (System/exit 0))))

(defmethod interp pairC [expr]
  (pairV. (interp (interp-inputs. (.fst (.expr expr)) (.env expr)))
          (interp (interp-inputs. (.snd (.expr expr)) (.env expr)))))

(defmethod interp fstC [expr]
  (if (instance?  pairV (interp (interp-inputs. (.p (.expr expr)) (.env expr))))

  (interp (interp-inputs. (.fst (.p (.expr expr))) (.env expr)))

  ((println "error. Not a pair")
  (System/exit 0))))

(defmethod interp sndC [expr]
(if (instance?  pairV (interp (interp-inputs. (.p (.expr expr)) (.env expr))))

  (interp (interp-inputs. (.snd (.p (.expr expr))) (.env expr)))

  ((println "error. Not a pair")
  (System/exit 0))))




;Method that prints out inside of Value expression
(defmulti pretty-print class)

(defmethod pretty-print numV [expr]
  (str (.n expr)))

(defmethod pretty-print boolV [expr]
  (str (.b expr)))

(defmethod pretty-print closV [expr]
  (str "closure " (.n expr)
                  (.body expr)))

(defmethod pretty-print pairV [expr]
  (str "pair (" (pretty-print (.fst expr)) ","
                (pretty-print (.snd expr)) " )"))

(defmethod pretty-print listV [expr]
(if (empty? (.l expr))
  (str "[]")
  (str "[" (pretty-print (first (.l expr))) ","
              (pretty-print (listV. (rest (.l expr)))) "]")))



;;we have different test cases in this comment
;;we've implemented pairs and an absolute value types as well, however,
;;we were not given a parse for either since they weren't mandatory
;;we also tried letrec but couldn't fully complete it
(comment

  (defmethod interp letrecC [expr]
    (let [b (box. -1)]
      (let [new-env (assoc (.env expr) (.id (.expr expr)) b)]
        (.setX b (interp (interp-inputs. (.rhs (.expr expr)) new-env)))
        (interp (interp-inputs. (.body (.expr expr)) new-env)))))

  (definterface Ibox
    (getX [])
    (setX [v]))

  (deftype box [^{:volatile-mutable true} x]
    (getX [_] x)
    (setX [this v] (set! x v)))


  (println (interp (interp-inputs. (letrecC. 'fact (lamC. 'n (ifC. (equalC. (idC. 'n) (numC. 0))
                                                                   (numC. 1)
                                                                   (multC. (idC. 'n) (appC. (idC. 'fact)
                                                                                            (plusC. (idC. 'n) (numC. -1))))))
                                             (appC. (idC. 'n) (numC. 5))) {})))





(println (interp (interp-inputs. (pairC. (numC. 7) (numC. 11)) {})))

(println (.n (interp (interp-inputs. (fstC. (pairC. (numC. 11) (numC. 7))) {}))))

(println (interp (interp-inputs. (lamC. 'x (ifC. (equalC. (idC. 'x) (numC. 7))
          (multC. (idC. 'x) (numC. 11))
          (multC. (idC. 'x) (idC. 'x)))) {})))
(println (.n (interp (interp-inputs. (appC. (lamC. 'x (ifC. (equalC. (idC. 'x) (numC. 7))
          (multC. (idC. 'x) (numC. 11))
          (multC. (idC. 'x) (idC. 'x)))) (numC. 7)) {}))))

(println (.n (interp (interp-inputs. (appC. (lamC. 'x (plusC. (idC. 'x) (numC. 7))) (numC. 11)) {}))))

(println (interp (interp-inputs. (firstC. (list 1 2 3 4)) {})))

(println (interp (interp-inputs. (restC. (list 1 2 3 4)) {})))

(println (interp (interp-inputs. (consC. 7 (list 1 2 3 4)) {})))

(println (interp (interp-inputs. (emptyC?. (list 1 2 3 4)) {})))

(println (.n (interp (interp-inputs. (letC. 'x (plusC. (numC. 5) (numC. 5)) (idC. 'x)) {}))))

(println (.n (interp (interp-inputs. (letC. 'x (numC. 7) (idC. 'x)) {}))))

(println (.n (interp (interp-inputs. (letC. 'x (plusC. (numC. 7) (numC. 11)) (plusC. (idC. 'x) (numC. -11))) {}))))

(println (interp (interp-inputs. (emptyC. (list 1 2 3 4)) {})))

(println (interp (interp-inputs. (lamC. ['x] [(plusC. (idC. 'x) (numC. 7))]) {})))

(println (.n (interp (interp-inputs. (plusC. (numC. 7) (numC. 11)) {}))))

(println (.b (interp (interp-inputs. (trueC.) {}))))

(println (.b (interp (interp-inputs. (equalC. (numC. 7) (numC. 11)) {}))))

(println (.n (interp (interp-inputs. (ifC. (equalC. (numC. 7) (numC. 11))
          (numC. 4)
          (numC. 5)) {}))))

(println (.n (interp (interp-inputs. (absC. (numC. -7)) {}))))

(println (.n (interp (interp-inputs. (absC. (plusC. (numC. 7) (numC. -11))) {}))))

(println (interp (interp-inputs. (lamC. 'x (ifC. (equalC. (idC. 'x) (numC. 7))
          (multC. (idC. 'x) (numC. 11))
          (multC. (idC. 'x) (idC. 'x)))) {})))

)




;;create like a package that has two things
;;can define new type has two field (expr and env)



  (defn -main [greeting]
  (println  (pretty-print (interp (interp-inputs. (parse (get (json/read-str (slurp greeting) :key-fn keyword) :prog)
    ) {} )))))





;SIDENOTES

;big if statements to determine which variant it is
;use map instead of creating new data type
;need to find a way to distinguish, like a key value
;define type for each variant
;we need to figure out which variant/type it is
;need a value datatype
;we give thing to match (different interps for all variants)
;get value
;.x where x is the name of param/field we wanna get out

;;field followed by the oject to get its field
