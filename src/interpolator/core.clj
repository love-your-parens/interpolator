(ns interpolator.core
  (:require [clojure.string :as s]))

(defn fmt
  "String formatter, which extends clojure.core.format with support for string interpolation.
   Any names delimited within a #{} form will be resolved in the provided bindings, and injected into the string.
   See usage examples below."
  ([text]
   (fmt text {}))
  ([text bindings & args]
   (let [re #"(?<!\\)#\{(.*?)\}"
         interpolator #(let [v (second %)
                             rv (read-string v)]
                         (if (symbol? rv)
                           ((keyword v) bindings)
                           (eval rv)))]
     (apply format
            (-> text
                (s/replace re "%s")
                (s/replace "\\#" "#"))
            (into
             (mapv (memoize interpolator)
                   (re-seq re text))
             args)))))

(defmacro fmt-let
  "Convenience macro for fmt, allowing you to provide bindings in a let-like format."
  [bindings text]
  `(fmt ~text ~(into {} (for [[k v] (partition 2 bindings)] [(keyword k) v]))))

;; Usage examples
(comment
  (fmt "Hello there!")
  (fmt "I'm your #{name} #{surname}!"
       {:name "string" :surname "interpolator"})
  (fmt "You can escape \\#{the delimiter} like this")
  (fmt-let [what "bindings"
            how "with let-like forms"]
           "You can provide #{what} #{how}!")
  (fmt (clojure.string/upper-case (str "I evaluate my input"))) 
  (fmt "I can also behave like #{function}: %d %.02f" {:function "format"} 7 8.1)
  (fmt "I will evaluate lists: 2 + 2 = #{(+ 2 2)}")
  (let [a 1] (fmt "But I *can't* evaluate local vars: #{(+ 1 a)}")) 
  )
