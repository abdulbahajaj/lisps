(ns lisps.cont-interpreter)

(defn wrong [err-message]
  (throw (RuntimeException. err-message)))

(declare beval)

;; Resuming from conts
(defmulti resume (fn [cont & _] (:type cont)))
(defmethod resume :default [cont cond-val] (wrong "Unknown cont"))

;; Helpers
(defn atom? [exp]
  (not (list? exp)))

(defn pair? [exp]
  (if (atom? exp) false
      (> (count exp) 1)))

(defn null? [exp]
  (= exp nil))

;; 3.2.3 Quoting
(defn eval-quote [exp env cont]
  (resume cont exp))

;; 3.2.4 Alternatives
(defn eval-if [cond true-body false-body env cont]
  (beval cond env {:type ::if-cont
                   :env env
                   :parent cont
                   :if-true true-body
                   :if-false false-body}))

(defmethod resume ::if-cont [cont cond-val]
  (beval (if cond-val (:if-true cont) (:if-false cont))
         (:env cont)
         (:parent cont)))

;; Sequence 3.2.5
(defn eval-begin [exp env cont]
  (if (atom? exp)
    (resume cont '())
    (if (> (count exp) 1)
      (beval (first exp) env {:type ::begin-cont
                              :parent cont
                              :rest (rest exp)
                              :env env})
      (beval (first exp) env cont))))

(defmethod resume ::begin-cont [cont v]
  (eval-begin (:rest cont)
              (:env cont)
              (:parent cont)))

;; 3.2.6 Variable Environment


(defmulti lookup (fn [env & _] (:type env)))
(defmethod lookup :default [env symbol cont] (wrong "Not an env"))

(defmethod lookup ::null-env [env symbol cont]
  (wrong "Unknown variable"))

(defmethod lookup ::full-env [env symbol cont]
    (lookup (:others env) symbol cont))

(defmethod lookup ::variable-env [env symbol cont]
  (if (= symbol (:name env))
    (resume cont (:value env)) 
    (lookup (:others env) symbol cont)))

(defn eval-var [exp env cont]
  (lookup env exp cont))

(defn eval-set! [symbol exp env cont]
  (beval exp env {:type ::set!-cont :parent cont :env env :symbol symbol}))

(defmulti update! (fn [env & _] (:type env)))
(defmethod update! :default [env symbol cont value](wrong "Not an env"))

(defmethod update! ::null-env [env symbol cont value]
  (wrong "Unknown variable"))

(defmethod update! ::full-env [env symbol cont value]
  (update! (:other env) symbol cont value))

(defmethod update! ::variable-env [env symbol cont value]
  (if (= symbol (:name env))
    (do (reset! (:value env) value)
        (resume cont value))
    (update! (:others env) symbol cont value)))

(defmethod resume ::set!-cont [cont value]
  (update! (:env cont) (:symbol cont) (:parent cont) value))

;; 3.2.7 Functions
(defn extend-env [env args values]
  (cond (and (pair? args) (pair? values))
        {:type ::variable-env
         :others (extend-env env (rest args) (rest values))
         :name (first args)
         :value (first values)}

        (and (= (count args) (count values) 0)) env

        (= (count args) (count values) 1)
        {:type ::variable-env
         :others env
         :name (first args)
         :value (first values)}

        (symbol? args) {:type ::variable-env
                         :others env
                         :name args
                         :value values}

        :else (wrong "Wrong arity")))

(defn eval-lambda [args exp env cont]
  (resume cont {:type ::function :args args :body exp :env env}))

(defn eval-app [func-exp arg-values env cont]
  (beval func-exp env {:type ::evfun-cont
                       :parent cont
                       :env env
                       :arg-values arg-values}))

(defn eval-args [arg-values env cont]
  (if (> (count arg-values) 0)
    (beval (first arg-values) env {:type ::argument-cont
                                    :parent cont
                                    :arg-values (rest arg-values)
                                    :env env})
    (resume cont '())))

(defmethod resume ::evfun-cont [cont func]
  (eval-args (:arg-values cont)
             (:env cont)
             {:type ::apply-cont
              :parent (:parent cont)
              :func func
              :env (:env cont)}))

(defmethod resume ::argument-cont [cont v]
  (eval-args (rest (:arg-values cont))
             (:env cont)
             {:type ::gather-cont
              :parent (:parent cont)
              :val v}))

(defmethod resume ::gather-cont [cont values]
  (resume (:parent cont) (cons (:val cont) values)))

(defmulti invoke (fn [callable & _] (:type callable)))
(defmethod invoke :default [] (wrong "Not a function"))
(defmethod invoke ::function [func vars env cont]
  (let [env (extend-env (:env func) (:args func) vars)]
    (eval-begin (:body func) env cont)))

(defmethod resume ::apply-cont [cont values]
  (invoke (:func cont) values (:env cont) (:parent cont)))

;; 3.2.2 Evaluate
(defn beval [exp env cont]
  (if (atom? exp)
    (if (symbol? exp)
      (eval-var exp env cont)
      (eval-quote exp env cont))
    (case (first exp)
      quote (eval-quote (second exp) env cont)
      if (eval-if (second exp) (nth exp 2) (nth exp 3) env cont)
      begin (eval-begin (rest exp) env cont)
      set! (eval-set! (second exp) (nth exp 2) env cont)
      lambda (eval-lambda (second exp) (-> exp rest rest) env cont)
      (eval-app (first exp) (rest exp) env cont))))

;; 3.3 Initializing the interpreter
(defmethod resume ::bottom-cont [cont v]
  (println v))

(defn brepl [& rest]
  (loop [env {:type ::null-env}
         cont {:type ::bottom-cont}]
    (println "\n----\n")
    (beval (read) env cont)
    (recur env cont)))

(brepl)
