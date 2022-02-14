(defmodule completion-util
  (export all))

(include-lib "apps/lfe-ls/include/lsp-model.lfe")

(defun find-completions-at (text position trigger)
  `(,(make-completion-item label #"defun"
                           kind (completion-item-kind-function))
    ,(make-completion-item label #"defmacro"
                           kind (completion-item-kind-function))
    ,(make-completion-item label #"defmodule"
                           kind (completion-item-kind-function))
    ,(make-completion-item label #"foobar"
                           kind (completion-item-kind-variable))
    ,(make-completion-item label #"io"
                           kind (completion-item-kind-module))))
