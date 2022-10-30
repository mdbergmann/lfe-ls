(defmodule hover-util-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

(include-lib "apps/lfe-ls/include/lsp-model.lfe")


(deftest get-docu
  (meck:new 'lfe_ls_docs)
  (meck:expect 'lfe_ls_docs 'h (lambda (_module _func)
                                 #"\n\e[;1m\tio\e[0m\n\n  This module provides an"))
  (let (((tuple 'ok doc) (hover-util:get-docu #"(io:format dkfjh" #(position 0 1))))
    (is-equal #"  This module provides an"
              (string:prefix doc #"\n\e[;1m\tio\e[0m\n\n")))

  (is (meck:validate 'lfe_ls_docs))
  (meck:unload 'lfe_ls_docs)
  )
