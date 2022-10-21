(defmodule hover-util-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")



(deftest is
  (is 'true)
  (is (not 'false))
  (is (not (not 'true))))
