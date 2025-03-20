(defn create [f]
      (fn [& args]
          (fn [vars]
              (apply f (mapv (fn [arg] (arg vars)) args)))))

(def add (create +))
(def subtract (create -))
(def multiply (create *))
(def negate (create -))
(def divide (create #(/ %1 (double %2))))
(def arcTan (create #(Math/atan %1)))
(def arcTan2 (create #(Math/atan2 %1 %2)))

(defn variable [name]
      (fn [env] (get env name)))
(defn constant [n]
      (fn [_] n))

(def funcOperation {'+ add, '- subtract, '* multiply, '/ divide, 'negate negate, 'atan arcTan, 'atan2 arcTan2})

(defn parse-tokens [list]
      (cond
        (number? list) (constant list)
        (list? list) (let [now (first list)] (apply (get funcOperation now) (mapv parse-tokens (rest list))))
        (symbol? list) (variable (name list))
        ))

(defn parseFunction [expression]
      (parse-tokens (read-string expression)))
; ////////////////////
; HW 11

(defn proto-get
      ([obj key] (proto-get obj key nil))
      ([obj key default]
       (cond
         (contains? obj key) (obj key)
         (contains? obj :prototype) (proto-get (obj :prototype) key default)
         :else default)))

(defn field  
      "Creates field"
      [key] (fn
              ([this] (proto-get this key))
              ([this def] (proto-get this key def))))


(defn proto-call
      "Calls object method respecting the prototype chain"
      [this key & args]
      (apply (proto-get this key) this args))


(defn method 
      "Creates method"
      [key] (fn [this & args] (apply proto-call this key args)))

(defn constructor
      "Defines constructor"
      [ctor prototype]
      (fn [& args] (apply ctor {:prototype prototype} args)))

(def val (field :val)) ;_x

(def evaluate (method :evaluate))
(def toString (method :toString))


(def VarProto
  {
   :evaluate (fn [this vars] (vars (val this)))
   :toString (fn [this] (val this))
   })

(defn Var [this val]
      (assoc this :val val)
      )

(def Variable (constructor Var VarProto))

(def ConstProto
  {
   :evaluate (fn [this vars] (val this) )
   :toString (fn [this] (str (val this)))
   })

(defn Const [this val]
      (assoc this :val val))

(def Constant (constructor Const ConstProto))

(def FuncProto
  {
   :evaluate (fn [this vars] (apply (:func this) (mapv (fn [arg1] (evaluate arg1 vars)) (:args this))))  
   :toString (fn [this] (str "(" (:sign this) " " (clojure.string/join " " (mapv #(toString %) (:args this))) ")")) 
   })

(defn AbstractOperation [func sign]
      (fn [& args]
          {
           :func func :sign sign :args args :prototype FuncProto
           })
      )

(def Add (AbstractOperation + '+))
(def Subtract (AbstractOperation - '-))
(def Multiply (AbstractOperation * '*))
(def Divide (AbstractOperation #(/ %1 (double %2)) '/))
(def Negate (AbstractOperation - 'negate))
(def Sinh (AbstractOperation #(Math/sinh %1) 'sinh))
(def Cosh (AbstractOperation #(Math/cosh %1) 'cosh))

(def funcOperation {'+ Add, '- Subtract, '* Multiply, '/ Divide, 'negate Negate, 'sinh Sinh, 'cosh Cosh})

(defn parse-tokens [list]
      (cond
        (number? list) (Constant list)
        (list? list) (let [now (first list)] (apply (get funcOperation now) (mapv parse-tokens (rest list))))
        (symbol? list) (Variable (name list))
        ))


(defn parseObject [expression]
      (parse-tokens (read-string expression)))


