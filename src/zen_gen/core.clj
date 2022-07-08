(ns zen-gen.core
  (:require
   [zen.core]
   clojure.core.reducers)
  (:import
   [java.util.concurrent ThreadLocalRandom]
   [com.mifmif.common.regex Generex]))

(defmulti generate
  (fn [context schema]
    (:type schema))
  :default 'zen/schema)

(defn- nth-check [context {nth :nth min-items :maxItems :or {min-items 0}}]
  (when nth
    {:nth-minItems (if-let [nth-max (->> nth keys (apply max))]
                     (if (> nth-max min-items)
                       (inc nth-max)
                       min-items)
                     min-items)
     :nth-map (into {} (map (fn [[i schema]] {i (generate context schema)}) nth))}))

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
      (if (and (or (contains? (:require schema) k)
                   (generate context {:type 'zen/boolean}))
               (not=  \_ (first (name k)))
               (not=  :extension k)
               (not=  :modifierExtension k))
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
    (cond->
        (->> (:confirms schema)
             (remove #(= % 'hl7-fhir-r4-core.Extension/schema #_(:zen/name schema)))
             (map #(zen.core/get-symbol context %))
             (apply merge)
             (generate context))
      (:zen.fhir/reference schema)
      (assoc :type
             (some->> (:zen.fhir/reference schema)
                      (:refers)
                      (map #(zen.core/get-symbol context %))
                      (keep :zen.fhir/type)
                      (seq)
                      (rand-nth))))))

(defmethod generate 'zen/vector
  [context {:keys [minItems maxItems every]
            :or {minItems 0 maxItems 10 every {:type 'zen/any}}
            :as schema}]
  (let [{:keys [nth-minItems nth-map]
         :or   {nth-minItems minItems}} (nth-check context schema)
        seq- (->> (repeatedly #(generate context every))
                  (take (generate context {:type 'zen/integer :min nth-minItems :max maxItems})))]
    (vec (if nth-map
           (->> nth-map
                (merge (zipmap (range) seq-))
                (sort-by first)
                (mapv second))
           seq-))))

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


(defmethod generate 'zen/list
  [context {:keys     [every minItems maxItems]
            :or       {every {:type 'zen/integer}
                       minItems  1
                       maxItems 10}
            :as schema}]
  (let [{:keys [nth-minItems nth-map]
         :or   {nth-minItems minItems}} (nth-check context schema)
        seq- (->> (repeatedly #(generate context every))
                  (take (generate context {:type 'zen/integer :min nth-minItems :max maxItems})))]
    (apply list (if nth-map
                  (->> nth-map
                       (merge (zipmap (range) seq-))
                       (sort-by first)
                       (map second))
                  seq-))))

(defmethod generate 'zen/case
  [context {case :case}]
  (some->> (map :then case)
           (filter (comp not :fail))
           seq
           rand-nth
           (generate context)))

(comment
  (def zen-context
    (zen.core/new-context {:paths [(str #_(System/getProperty "user.dir")
                                       (str "/home/veschin/work/sansara/box/zrc" #_ #_"fhir.edn" "aidbox.edn")
                                       #_ "/zrc")]}))

  (zen.core/read-ns zen-context 'hl7-fhir-r4-core)

  (keys (:symbols @zen-context))
  (zen.core/get-symbol zen-context 'hl7-fhir-r4-core.value-set.device-type/valuse-set)


  (generate zen-context {:confirms #{'hl7-fhir-r4-core.Patient/schema}}) 
  (generate zen-context {:confirms #{'hl7-fhir-r4-core.Attachment/schema}}) 
  (zen.core/read-ns zen-context 'aidbox)

  (-> @zen-context :ns #_(get 'zen) keys)

  (:zen/name (zen.core/get-symbol zen-context 'hl7-fhir-r4-core.Element/schema))
  (generate zen-context
            {:confirms #{'hl7-fhir-r4-core.Attachment/schema}} ) 

 (generate zen-context {:confirms #{'aidbox/config}})

  ;; ?
  (take
   100
   (repeatedly
    #(generate {} {:type 'zen/string :regex "([0-9]([0-9]([0-9][1-9]|[1-9]0)|[1-9]00)|[1-9]000)(-(0[1-9]|1[0-2])(-(0[1-9]|[1-2][0-9]|3[0-1])(T([01][0-9]|2[0-3]):[0-5][0-9]:([0-5][0-9]|60)(\\.[0-9]+)?(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00)))?)?)?"})))

  (def aidbox (clojure.edn/read-string (slurp "/home/veschin/work/sansara/box/zrc/aidbox.edn")))

  (->> (slurp "/home/veschin/work/sansara/box/zrc/aidbox.edn")
       (re-seq #"(zen\/\w+)")
       (map first)
       frequencies
       (sort-by second >))

  ;;

  )
