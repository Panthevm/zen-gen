(ns zen-gen.core
  (:require
   [zen.core])
  (:import
   [java.util.concurrent ThreadLocalRandom]
   [com.mifmif.common.regex Generex]))

(defmulti generate
  (fn [context schema]
    (:type schema))
  :default 'zen/schema)

(defmethod generate 'zen/boolean
  [context schema]
  (.nextBoolean (ThreadLocalRandom/current)))

(defmethod generate 'zen/integer
  [context schema]
  (let [min-value (or (:min schema) (inc Integer/MIN_VALUE))
        max-value (inc (or (:max schema) (dec Integer/MAX_VALUE)))]
    (if (= min-value max-value)
      min-value
      (.nextInt (ThreadLocalRandom/current) min-value max-value))))

(defmethod generate 'zen/number
  [context schema]
  (if (or (some double? [(:min schema) (:max schema)])
          (generate context {:type 'zen/boolean}))
    (let [min-value (or (:min schema)
                        (inc Double/MIN_VALUE))
          max-value (inc (or (:max schema)
                             (dec Double/MAX_VALUE)))]
      (if (= min-value max-value)
        min-value
        (.nextDouble (ThreadLocalRandom/current) min-value max-value)))
    (generate context {:type 'zen/integer
                       :min (:min schema)
                       :max (:max schema)})))

(defmethod generate 'zen/string
  [context schema]
  (let [min-length (get schema :minLength 0)
        max-length (get schema :maxLength 10)]
    (->>
     (if (:regex schema)
       (-> (Generex. (:regex schema)) .random seq) 
       (repeatedly
        #(generate context
                     {:type 'zen/integer
                      :min  32 
                      :max  126})))
     (take (generate context {:type 'zen/integer :min min-length :max max-length}))
     (map char)
     (apply str))))

(defmethod generate 'zen/keyword
  [context schema]
  (keyword
   (when (generate context {:type 'zen/boolean})
     (generate context {:type 'zen/string :regex "\\w\\w+"}))
   (generate context {:type 'zen/string :regex "\\w\\w+"})))

(defmethod generate 'zen/symbol
  [context schema]
  (symbol
   (when (generate context {:type 'zen/boolean})
     (generate context {:type 'zen/string}))
   (generate context {:type 'zen/string})))

(defmethod generate 'zen/datetime
  [context schema]
  (generate context
            {:type      'zen/string
             :minLength 19
             :maxLength 19
             :regex     "\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}"}))

(defmethod generate 'zen/date
  [context schema]
  (generate context
            {:type      'zen/string
             :minLength 10
             :maxLength 10
             :regex     "\\d{4}-\\d{2}-\\d{2}"}))

(defmethod generate 'zen/map
  [context schema]
  (->>
   schema
   (:keys)
   (reduce
    (fn [acc [k v]]
      (if (or (contains? (:required schema) k)
              (generate context {:type 'zen/boolean}))
        (assoc acc k (generate context v))
        acc))
    (into {} (generate context (dissoc schema :type))))))

(defmethod generate 'zen/schema
  [context schema]
  (cond
    (:const schema)
    (-> schema :const :value)
    (:enum schema)
    (->>
     (rand-nth (:enum schema))
     (:value))
    (:confirms schema)
    (->> (:confirms schema)
         ;; (remove #(= % 'hl7-fhir-r4-core.Extension/schema #_(:zen/name schema)))
         (map #(zen.core/get-symbol context %))
         (apply merge)
         (generate context))))

(defmethod generate 'zen/vector
  [context schema]
  (let [min-items (or (:minItems schema) 0)
        max-items (or (:maxItems schema) 8)]
    (->>
     (repeatedly #(generate context (:every schema)))
     (take (generate context {:type 'zen/integer :min min-items :max max-items}))
     (vec))))

(defmethod generate 'zen/set
  [context {:keys     [every minItems maxItems
                           superset-of subset-of]
            :or        {every {:type 'zen/integer}
                        minItems  0
                        maxItems 5}}]
  (if subset-of
    (->> (generate context {:type 'zen/integer :min 0 :max (count subset-of)})
         (range)
         (map (fn [_] (rand-nth (vec subset-of))))
         (set))
    (let [seq- (->>
                (repeatedly #(generate context every))
                (take (generate context {:type 'zen/integer :min minItems :max maxItems})))]
      (set
       (if superset-of
         (concat superset-of seq-)
         seq-)))))

(defmethod generate 'zen/any
  [context schema]
  (generate context {:type (rand-nth (keys (methods generate)))}))






(comment
  (def zen-context
    (zen.core/new-context {:paths [(str (System/getProperty "user.dir") "/zrc")]}))

  (zen.core/read-ns zen-context 'my-zen-ns)

  (:zen/name (zen.core/get-symbol zen-context 'hl7-fhir-r4-core.Element/schema))
  (generate zen-context
            {:confirms #{'hl7-fhir-r4-core.Attachment/schema}} ) 

  ;; ?
  (take
   100
   (repeatedly
    #(generate {} {:type 'zen/string :regex "([0-9]([0-9]([0-9][1-9]|[1-9]0)|[1-9]00)|[1-9]000)(-(0[1-9]|1[0-2])(-(0[1-9]|[1-2][0-9]|3[0-1])(T([01][0-9]|2[0-3]):[0-5][0-9]:([0-5][0-9]|60)(\\.[0-9]+)?(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00)))?)?)?"})))
  )
