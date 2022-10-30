(defmodule hover-util-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

(include-lib "apps/lfe-ls/include/lsp-model.lfe")


(deftest get-docu
  (is-equal `#(ok #"") (hover-util:get-docu #"foo:bar" #(position 0 1)))
  )