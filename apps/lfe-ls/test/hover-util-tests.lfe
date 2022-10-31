(defmodule hover-util-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

(include-lib "apps/lfe-ls/include/lsp-model.lfe")


(deftest get-docu--char-on-module
  (meck:new 'lfe_ls_docs)
  (meck:expect 'lfe_ls_docs 'h (lambda (module func)
                                 (list_to_binary
                                  (lists:flatten
                                         (lfe_io:format1 "~s:~s" `(,module ,func))))))
  (let (((tuple 'ok doc) (hover-util:get-docu #"(io:format dkfjh" #(position 0 1))))
    (is-equal #"io:format" doc))
  (is (meck:validate 'lfe_ls_docs))
  (meck:unload 'lfe_ls_docs))

(deftest get-docu--char-on-func
  (meck:new 'lfe_ls_docs)
  (meck:expect 'lfe_ls_docs 'h (lambda (module func)
                                 (list_to_binary
                                  (lists:flatten
                                         (lfe_io:format1 "~s:~s" `(,module ,func))))))
  (let (((tuple 'ok doc) (hover-util:get-docu #"(io:format dkfjh" #(position 0 4))))
    (is-equal #"io:format" doc))
  (is (meck:validate 'lfe_ls_docs))
  (meck:unload 'lfe_ls_docs))

(deftest get-docu--char-on-func--other-string
  (meck:new 'lfe_ls_docs)
  (meck:expect 'lfe_ls_docs 'h (lambda (module func)
                                 (list_to_binary
                                  (lists:flatten
                                         (lfe_io:format1 "~s:~s" `(,module ,func))))))
  (let (((tuple 'ok doc) (hover-util:get-docu #"io:format" #(position 0 4))))
    (is-equal #"io:format" doc))
  (is (meck:validate 'lfe_ls_docs))
  (meck:unload 'lfe_ls_docs))

(deftest get-docu--char-on-func--other-string2
  (meck:new 'lfe_ls_docs)
  (meck:expect 'lfe_ls_docs 'h (lambda (module func)
                                 (list_to_binary
                                  (lists:flatten
                                         (lfe_io:format1 "~s:~s" `(,module ,func))))))
  (let (((tuple 'ok doc) (hover-util:get-docu #"    io:format    " #(position 0 8))))
    (is-equal #"io:format" doc))
  (is (meck:validate 'lfe_ls_docs))
  (meck:unload 'lfe_ls_docs))

(deftest get-docu--char-on-func--other-string3
  (meck:new 'lfe_ls_docs)
  (meck:expect 'lfe_ls_docs 'h (lambda (module func)
                                 (list_to_binary
                                  (lists:flatten
                                         (lfe_io:format1 "~s:~s" `(,module ,func))))))
  (let (((tuple 'ok doc) (hover-util:get-docu #"  (io:format)" #(position 0 11))))
    (is-equal #"io:format" doc))
  (is (meck:validate 'lfe_ls_docs))
  (meck:unload 'lfe_ls_docs))

(deftest get-docu--error-unrecognized-module
  (meck:new 'lfe_ls_docs)
  (meck:expect 'lfe_ls_docs 'h (lambda (_) '#(error foo)))
  (is-equal '#(error foo) (hover-util:get-docu #"  dkfjh   " #(position 0 3)))
  (is (meck:validate 'lfe_ls_docs))
  (meck:unload 'lfe_ls_docs))

(deftest get-docu--char-pos-empty
  (let (((tuple 'ok doc) (hover-util:get-docu #"  io:format dkfjh" #(position 0 1))))
    (is-equal #"" doc)))

(deftest get-docu--empty-text
  (let (((tuple 'ok doc) (hover-util:get-docu #"" #(position 0 0))))
    (is-equal #"" doc)))
