(ns evening.runtime
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as spec])
  (:gen-class))


; Matchers are like regexes for Clojure maps (with keyword keys).
; Matchers "match" against other maps when they are a submap of the given map.
; They can also have variables in them, so they can match against a wider variety
; of maps.
; For instance, the matcher {:foo {:var :bar}} matches against:
;   - {:foo 3}
;   - {:foo "bar"}
;   - {:foo {:baz :quux}}
;   - {:bar 3 :foo 4}
; It won't match against:
;   - {:bar 5}
;   - {:bar :foo}
;
; The function "bindings" extracts variable bindings from the match.
; If there is no match, will return nil. If there is a match but there were no
; variables in the matcher, "bindings" will simply return an empty map.
; Using the previous example, here would be the corresponding reults of "bindings":
;   - {:bar 3}
;   - {:bar "bar"}
;   - {:bar {:baz :quux}}
;   - {:bar 4}
;   - nil
;   - nil

(defn- matcher-var-key-value?
  "Given a key and a value, checks if they form a matcher-var shape"
  [key value]
  (and
    (some? key) 
    (some? value)
    (= :var key)
    (keyword? value)))


(defn matcher-var?
  "Checks if a value is in the shape of a variable in a matcher"
  [x]
  (cond 
    ; x can be a map...
    (map? x)
    (and 
      (= 1 (count x))
      ; (first x) takes a map that looks like {foo bar} 
      ; and turns it into a vector that looks like [foo bar]
      (let [key (first (first x))
            value (second (first x))]
        (matcher-var-key-value? key value)))
    ; ... or a vector
    (vector? x)
    (and 
      (= 2 (count x))
      (let [key (first x)
            value (second x)]
        (matcher-var-key-value? key value)))
    :else false))


(defn ground?
  "Checks if a matcher value is a 'ground', which means that it 
  and its children contain no matcher variables"
  [x]
  (loop [kvps x]
    (let [first-kvp (cond (map? kvps) (first kvps)
                          (vector? kvps) kvps
                          :else nil)]
      (if (nil? first-kvp) 
        false
        (let [key (first first-kvp)
              value (second first-kvp)]
          (if (or (matcher-var? key)
                  (matcher-var? value))
            false
            (if (keyword? key)
              (if (not (associative? value))
                true
                (recur value)))))))))


(defn submap?
  "Checks if one map's key-value pairs are a subset of another's"
  [sub super]
  (= (merge super sub) super))


(defn merge-variable-binding
  [current new]
  (when (and (some? new) (some? current))
    (let [key (first new)
          value (second new)]
      (when (some? value)
        (if (some? (key current))
          (when (= (key current) value)
            current)
          (assoc current key value))))))


(defn merge-variable-bindings
  [current new]
  (when (some? new) (reduce merge-variable-binding current new)))


(defn what-is
  [pred val]
  (pred val))


(declare bindings-aux)
(defn match-single-kvp
  "Given a single key-value-pair of a matcher and 
  some data to match against, gets any variable bindings 
  from that match"
  [[matcher-key matcher-value] to-match current-vars]
  (when (keyword? matcher-key)
    (when-let [corresponding-value (matcher-key to-match)]
      (condp what-is matcher-value
        matcher-var? {(:var matcher-value) corresponding-value}
        map? (bindings-aux current-vars matcher-value corresponding-value)
        (when (= corresponding-value matcher-value) {})))))


(defn bindings-aux
  "Extracts variables from matcher on data and merges them into a map of already known variables"
  [current-vars matcher to-match]
  (reduce (fn [state it] 
            (->> (match-single-kvp it to-match state)
                 (merge-variable-bindings state)))
    current-vars
    matcher))


(defn bindings
  "Extracts variables from matchers on data"
  [matcher to-match]
  (bindings-aux {} matcher to-match))


(defn all-bindings-single-matcher
  "Extracts all sets of variable bindings from a single matcher against a set of maps"
  [matcher maps]
  (some->> (map (fn [m] (bindings matcher m)) maps)
           (#(when (some some? %) %))
           (filter seq)))


(defn combinations
  "Given some collections, makes new collections by 
  conjoining each element from each other collection"
  [colls]
  (reduce (fn [state it]
            (mapcat (fn [el]
                   (map (fn [state-coll]
                             (conj state-coll el))
                           state))
                 it))
          (conj (empty colls) (empty colls))
          colls))


(defn all-seqs-not-nil
  "Given a collection of collections, returns nil if any 
  of the individual collections are nil and returns the 
  larger collection without any empty ones."
  [colls]
  (when (every? some? colls) (filter seq colls)))

  
(defn log 
  [s x]
  (do (print (str s ": "))
      (prn x)
      x))
  

(defn all-bindings
  "Given some matchers and some data to match against, 
  extracts the set of all variable bindings
  that fits all the matchers."
  [matchers maps]
  (some->> (map #(all-bindings-single-matcher % maps) matchers)
           (all-seqs-not-nil)
           (combinations)
           (map #(reduce merge-variable-bindings {} %))
           (filter seq)
           (into #{})))


(defn variable-substitution
  "Given a matcher and a mapping of variables to values, 
  replaces all variables in the matcher with their corresponding values.
  Throws an exception if a variable cannot be assigned a value."
  [matcher var-values]
  (condp what-is matcher
    matcher-var? (if-let [var-val ((:var matcher) var-values)] 
                  var-val
                  (throw (ex-info "Cannot match variable!"
                            {:trying-to-match (:var matcher)
                             :mappings var-values})))
    map? (into {} (map (fn [[k v]] [k (variable-substitution v var-values)]) matcher))
    matcher))


(defn variable-substitutions
  "Given a multiple sets of variable bindings and a set of of matchers,
  generates a set of maps with the variables substituted in (see `matcher-var?`).
  Creates a map with each variable binding map as a key to a list of all the 
  substituted matchers."
  [var-bindings matchers]
  (into {} (map (fn [single-binding-set] 
    [single-binding-set (into #{} (map #(variable-substitution % single-binding-set) matchers))]) var-bindings)))


(defn maybe-add-fact
  [facts side-effect val]
  (do (when (not (contains? facts val)) (side-effect val))
      val))


(defn infer
  "Given a some premises (as matchers), conclusions (as a map of matchers to side-effects),
  and facts (maps that can be matched against using 'bindings'), provides all new facts
  that can be inferred."
  [premises conclusions facts]
  (some->> (all-bindings premises facts)
           (mapcat #(map (fn [[matcher side-effect]] 
                            (maybe-add-fact facts side-effect (variable-substitution matcher %))) conclusions))
           (into #{})))


(defrecord Rule [premises conclusions])

(spec/def ::keyword-keyed-map (spec/map-of keyword? (spec/or :map ::keyword-keyed-map :anything-else any?)))
(spec/def ::premises (spec/coll-of ::keyword-keyed-map :type set? :min-count 1))
(spec/def ::conclusions (spec/map-of ::keyword-keyed-map (spec/fspec :args (spec/cat :data any?) :ret any?)))
(spec/def ::rule (spec/keys :req-un [::premises ::conclusions]))


(defrecord KnowledgeBase [facts rules])

(spec/def ::facts (spec/coll-of ::keyword-keyed-map :type set?))
(spec/def ::rules (spec/coll-of ::rule :type set?))
(spec/def ::knowledge-base (spec/keys :req-un [::facts ::rules]))


(defn fix-from
  "Finds the fixed point of a function given an initial 'seed' value."
  [f init]
  (loop [prev init 
         next (f init)]
    (if (= prev next) 
      next
      (recur next (f next)))))


(defn infer-step
  "Given a KnowledgeBase, infers new facts that can be known from one
  inference 'step.' Any facts inside the KnowledgeBase returned from this
  function must have either been known from the original or been derived
  only from facts and rules known in the original."
  [kb]
  (let [rules (:rules kb)
        new-facts (apply set/union
                    (map #(infer (:premises %) 
                                 (:conclusions %) 
                                 (:facts kb))
                         rules))]
    (->KnowledgeBase (set/union (:facts kb) new-facts) (:rules kb))))


(defn infer-all
  "Given a KnowledgeBase, infers all possible new facts from what's known
  and returns a new KnowledgeBase with all the new facts."
  [kb]
  (fix-from infer-step kb))
