# protocol55.spec.permutations

Confirm all permutations of your specs have been checked.

## Rationale

Often functions require specs that meet many different outputs. Each branch or
permutation is a valid path, and because of this it is possible that when
checking your functions only some paths have been actually checked.

## Usage

`protocol55.spec.permutations/check-fn` is like
`clojure.spec.test.alpha/check-fn` but also reports whether any permutations of
`:ret` have been unchecked.


```
(require '[protocol55.spec.permutations :as p])

(p/check-fn my-function (get-spec `my-function))

; success

{... :protocol55.spec.permutations.check {:result true}}

; failure

{... :protocol55.spec.permutations.check {:result {:unchecked [...]}}}
```

## Limitations

This library uses `spec-tools.parse/parse-spec` when building permutations. The
basic types returned are supported, but custom specs will not.

## TODO

- Support regex specs
