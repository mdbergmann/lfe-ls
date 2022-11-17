(defmodule compile-util-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

(include-lib "apps/lfe-ls/include/lsp-model.lfe")

(deftest compile--ok--no-include
  (let ((`#(ok ,cwd) (file:get_cwd)))
    (is-equal #(ok ()) (compile-util:compile-file
                                     (++ cwd "/compile-tmpls/ok-no-include.lfe")
                                     cwd))))

(deftest compile--error--no-out-dir
  (let ((`#(ok ,cwd) (file:get_cwd)))
    (is-equal (tuple
               'ok
               (list
                (make-diagnostic-item
                 range (line-to-range 0)
                 severity (diag-severity-error)
                 source #"lfe_lint"
                 message #"#(undefined_function #(my-fun 1))")))
              (compile-util:compile-file
                            (++ cwd "/compile-tmpls/error-no-include.lfe")
                            ""))))

(deftest compile--error--no-include
  (let ((`#(ok ,cwd) (file:get_cwd)))
    (is-equal (tuple
               'ok
               (list
                (make-diagnostic-item
                 range (line-to-range 0)
                 severity (diag-severity-error)
                 source #"lfe_lint"
                 message #"#(undefined_function #(my-fun 1))")))
              (compile-util:compile-file
                            (++ cwd "/compile-tmpls/error-no-include.lfe")
                            cwd))))

(deftest compile--error--include-doesnt-exist
  (let ((`#(ok ,cwd) (file:get_cwd)))
    (is-equal (tuple
               'ok
               (list
                (make-diagnostic-item
                 range (line-to-range 2)
                 severity (diag-severity-error)
                 source #"lfe_macro_include"
                 message #"#(no_include lib \"doesnt-exist.lfe\")")))
              (compile-util:compile-file
                            (++ cwd "/compile-tmpls/error-incl-not-exists.lfe")
                            cwd))))

(deftest compile--ok-unused-functions
  (let ((`#(ok ,cwd) (file:get_cwd)))
    (is-equal (tuple
               'fail
               (list
                (make-diagnostic-item
                 range (line-to-range 3)
                 severity (diag-severity-warn)
                 source #"erl_lint"
                 message #"#(unused_function #(foo 1))")
                (make-diagnostic-item
                 range (line-to-range 3)
                 severity (diag-severity-warn)
                 source #"erl_lint"
                 message #"#(unused_function #(foo2 0))")))
              (compile-util:compile-file
                            (++ cwd "/compile-tmpls/ok-incl-unused.lfe")
                            cwd))))
