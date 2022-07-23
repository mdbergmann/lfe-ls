(defmodule comp-ok-incl-unused
  (export (my-fun 1)))

(include-lib "compile-tmpls/include/some-funs.lfe")

(defun my-fun (arg1)
  "Foo")
