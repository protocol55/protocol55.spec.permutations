(ns protocol55.spec.permutations-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [protocol55.spec.permutations :as p]
            [spec-tools.parse :as parse]))

(s/def ::map-of (s/map-of ::bar ::bar))
(s/def ::and (s/and ::foo (s/conformer #(seq %))))
(s/def ::every (s/every ::foo))
(s/def ::merge (s/merge (s/keys :opt [::foo ::bar])
                        (s/keys :opt [::bar])))
(s/def ::tuple (s/tuple string? keyword? number?))
(s/def ::bar (s/or :b1 string? :b2 number?))
(s/def ::foo (s/or :a1 ::bar :a2 keyword?))
(s/def ::req keyword?)

(defmulti spec-multi :kind)

(defmethod spec-multi :a [_]
  (s/keys :req [::bar]))

(defmethod spec-multi :b [_]
  (s/keys :req [::foo]))

(s/def ::multi (s/multi-spec spec-multi :kind))

(s/def ::baz (s/keys :req [::req ::and] :opt [::foo ::tuple ::merge]
                      :req-un [::foo ::multi]
                      :opt-un [::tuple]))

; enable check
(s/check-asserts true)

(deftest test-spec-permutations
  (testing "kitchen sink"
    (is (= 26 (count (p/spec-permutations ::baz)))))
  (testing "multi-spec"
    (is (= 6 (count (p/spec-permutations ::multi))))))

(deftest test-unchecked
  (is (nil? (p/unchecked (p/spec-permutations ::multi)
                         [{::foo "string"}
                          {::foo 1}
                          {::foo :keyword}
                          {::bar "string"}
                          {::bar 1}])))
  (is (seq (p/unchecked (p/spec-permutations ::multi)
                         []))))

(defn assert- [x spec]
  (s/assert spec x))

(deftest test-check-fn
  (testing "passes"
    (is (-> (p/check-fn (fn [x] x)
                        (s/fspec :args (s/cat :x ::baz)
                                 :ret ::baz))
            (get-in [:protocol55.spec.permutations.check/ret :result])
            (assert- true?))))
  (testing "fails"
    (is (-> (p/check-fn (fn [x] x)
                        (s/fspec :args (s/cat :x ::baz)
                                 :ret ::baz)
                        {:clojure.spec.test.check/opts {:num-tests 1}})
            (get-in [:protocol55.spec.permutations.check/ret :result])
            (assert- #(contains? % :unchecked))))))
