(defmodule lsp-proc-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest produce-output
  (is-equal #"Bar" (lsp-proc:process-input #"Foo")))
