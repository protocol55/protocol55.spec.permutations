(ns protocol55.spec.permutations
  (:require [spec-tools.parse :as parse]
            [spec-tools.impl :as impl]
            [clojure.zip :as zip]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as st]))

(defn- get-type [x]
  (cond
    (vector? (:type x)) (first (:type x))
    :else (:type x)))

(defn- spec-zip [spec]
  (zip/zipper
    map?
    (some-fn (fn [m]
               (when (#{:tuple} (get-type m))
                 (map-indexed #(assoc %2 ::tuple-idx %1) (::parse/items m))))
             (fn [m]
               (when (#{:map-of} (get-type m))
                 (map #(assoc (% m) ::map-of-key %)
                      [::parse/key ::parse/value])))
             (fn [m]
               (when-some [key->spec (::parse/key->spec m)]
                 (map (fn [[k spec-name]]
                        (-> (parse/parse-spec spec-name)
                            (assoc ::map-key k)))
                      key->spec)))
             (fn [m]
               (when (#{:vector} (get-type m))
                 [(::parse/item m)]))
             (fn [m]
               (when (#{:multi-spec} (get-type m))
                 (map parse/parse-spec (vals (::parse/dispatch m)))))
              ::parse/items)
    nil
    (parse/parse-spec spec)))

(defn- full-path [loc]
  (conj (vec (zip/path loc)) (zip/node loc)))

(defn- leaf-paths [z]
  (->> z
       (iterate zip/next)
       (take-while (complement zip/end?))
       (filter (complement zip/children))
       (map full-path)))

(defmulti make-spec
  "Given spec parse info and a context containing subinfo parse maps returns
  a spec constructed from that info."
  (fn [info {:keys [subinfo] :as context}]
    (get-type info)))

(defmethod make-spec :or
  [_ {:keys [subinfo]}]
  (make-spec (first subinfo) {:subinfo (rest subinfo)}))

(defmethod make-spec :and
  [_ {:keys [subinfo]}]
  (make-spec (first subinfo) {:subinfo (rest subinfo)}))

(defmethod make-spec :map
  [{:keys [::parse/key->spec]} {:keys [subinfo]}]
  (let [{:keys [::map-key] :as branch-info} (first subinfo)]
    (s/and-spec-impl
      [map-key :spec]
      [(s/conformer #(if (contains? % map-key)
                       (get % map-key)
                       ::s/invalid))
       (make-spec branch-info {:subinfo (rest subinfo)})]
      nil)))

(defmethod make-spec :tuple
  [{:keys [::parse/items] :as info} {:keys [subinfo]}]
  (let [{:keys [::tuple-idx] :as branch-info} (first subinfo)
        [_ forms] (:type info)
        preds (-> (vec (repeat (count items) any?))
                  (assoc tuple-idx
                         (make-spec branch-info {:subinfo (rest subinfo)})))]
    (s/tuple-impl forms preds)))

(defmethod make-spec :vector
  [{:keys [::parse/item]} {:keys [subinfo]}]
  (impl/coll-of-spec (make-spec (first subinfo) {:subinfo (rest subinfo)}) []))

(defmethod make-spec :map-of
  [_ {:keys [subinfo]}]
  (let [{:keys [::map-of-key] :as branch-info} (first subinfo)]
    (apply impl/map-of-spec
           (cond-> [(make-spec branch-info {:subinfo (rest subinfo)}) any?]
             (#{::parse/key} map-of-key) (reverse)))))

(defmethod make-spec :multi-spec
  [_ {:keys [subinfo]}]
  (make-spec (first subinfo) {:subinfo (rest subinfo)}))

(defmethod make-spec :default
  [{:keys [spec] :as info} _]
  (or spec any?))

(defn- rebuild-spec [path]
  (make-spec (first path) {:subinfo (rest path)}))

(defn spec-permutations
  "Given a spec returns specs for each permutation of it."
  [spec]
  (->> (spec-zip spec)
       (leaf-paths)
       (filter (comp get-type last))
       (map rebuild-spec)))

(defn unchecked
  "Given a collection of specs (presumably permutations) and a collection of
  values returns all specs not found valid in values."
  [specs values]
  (reduce
    (fn [specs v]
      (let [res (map #(s/valid? % v) specs)]
        (->> (map #(when-not %1 %2) res specs)
             (keep identity)
             seq)))
    specs
    values))

(defn check-fn
  "Like clojure.spec.test.alpha/check-fn but also reports whether any permutations
  of :ret have been unchecked."
  ([f spec]
   (check-fn f spec nil))
  ([f spec opts]
   (let [ret-perms (spec-permutations (:ret spec))
         spec-args (:args spec)
         spec-ret (:ret spec)
         spec-fn (or (:fn spec) (constantly true))
         state (atom {:unchecked ret-perms})
         fspec (s/fspec :args spec-args
                        :ret (s/nonconforming spec-ret)
                        :fn (fn [{:keys [ret args]}]
                              (swap! state update :unchecked unchecked [ret])
                              (spec-fn {:args args :ret (s/conform spec-ret ret)})))
         result (st/check-fn f fspec opts)]
     (-> result
         (assoc :protocol55.spec.permutations.check/ret
                {:result (if-some [unchecked (seq (:unchecked @state))]
                           {:unchecked (vec unchecked)}
                           true)})))))
