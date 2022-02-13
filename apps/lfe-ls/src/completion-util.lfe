(defmodule completion-util
  (export all))

(include-lib "apps/lfe-ls/include/lsp-model.lfe")

(defun find-completions-at (text position trigger)
  `(,(make-completion-item label #"defun"
                           kind (completion-item-kind-function))))
