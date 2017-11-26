(ns tap.unify
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
      (let [key (first (first x)) ; (first x) takes a map that looks like {foo bar} and turns it into a vector that looks like [foo bar]
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


; (ground? [:foo :bar])
; (ground? [:foo {:var :bar}])
; (ground? {:foo {:var :bar}})

(defn submap?
  "Checks if one map's key-value pairs are a subset of another's"
  [sub super]
  (= (merge super sub) super))


(defn merge-variable-binding
  [current new]
  (when (some? new)
    (let [key (first new)
          value (second new)]
      (when (some? value)
        (if (some? (key current))
          (when (= (key current) value)
            current)
          (assoc current key value))))))


(defn merge-variable-bindings
  [current new]
  (reduce merge-variable-binding current new))


(defn bindings-aux-
  "Extracts variables from matcher on data and merges them into a map of already known variables" 
  [current-vars matcher to-match] 
  (let [ground-key-value-pairs (filter ground? matcher)
        variable-key-value-pairs (filter (comp not ground?) matcher)]
    ; if not all grounds can be matched and all keys of non-grounds can be matched,
    ; we can't match the matcher to the data, so return nil
    (when (submap? ground-key-value-pairs to-match)
      ; otherwise, for each value of the non-ground key-value pairs
      (->> (map (fn find-and-bind [[key-in-matcher val-in-matcher]] 
                    ; if it is in the form {:var <something>}
                    (if (apply matcher-var-key-value? (first val-in-matcher))
                      ; bind the variable <something> to the corresponding value in "to-match"
                      (let [var-name (second (first val-in-matcher))]
                        {var-name (key-in-matcher to-match)})
                      ; otherwise, recurse with "to-match" being the corresponding value in the current "to-match" and "matcher" as the value
                      (bindings-aux current-vars val-in-matcher (key-in-matcher to-match))))
                variable-key-value-pairs)
            (reduce merge-variable-bindings current-vars)))))


(defn bindings-
  "Extracts variables from matchers on data"
  [matcher data]
  (bindings-aux {} matcher data))


(defn what-is
  [pred val]
  (pred val))


(defn match-single-kvp
  "Given a single key-value-pair of a matcher and some data to match against, gets any variable bindings from that match"
  [[matcher-key matcher-value] to-match current-vars]
  (when (keyword? matcher-key)
    (when-let [corresponding-value (matcher-key to-match)]
      (condp what-is? matcher-value
        matcher-var? {(:var matcher-value) #{corresponding-value}}
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