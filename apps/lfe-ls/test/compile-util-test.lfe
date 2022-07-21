(defmodule compile-util-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest compile--ok--no-include
  (let ((`#(ok ,cwd) (file:get_cwd)))
    (is-match #(ok ()) (compile-util:compile-file
                        (++ cwd "/apps/lfe-ls/test/compile-tmpls/no-include.lfe")))))
