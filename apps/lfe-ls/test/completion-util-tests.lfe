(defmodule completion-util-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

;; your test code here

(deftest is
  (is 'true)
  (is (not 'false))
  (is (not (not 'true))))
