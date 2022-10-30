(defmodule hover-util-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

(include-lib "apps/lfe-ls/include/lsp-model.lfe")


(deftest get-docu-1
  (meck:new 'lfe_ls_docs)
  (meck:expect 'lfe_ls_docs 'h (lambda (module func)
                                 (list_to_binary
                                  (lists:flatten
                                         (lfe_io:format1 "~s:~s" `(,module ,func))))))
  (let (((tuple 'ok doc) (hover-util:get-docu #"(io:format dkfjh" #(position 0 1))))
    (is-equal #"io:format" doc))

  (is (meck:validate 'lfe_ls_docs))
  (meck:unload 'lfe_ls_docs))

(deftest get-docu--char-pos-empty
  (let (((tuple 'ok doc) (hover-util:get-docu #"  io:format dkfjh" #(position 0 1))))
    (is-equal #"" doc)))
