(defmodule completion-util-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "apps/lfe-ls/include/lsp-model.lfe")

(deftest find-completions--trigger-character
  (is-equal `(,(make-completion-item label #"defun"
                                     kind (completion-item-kind-function)))
            (completion-util:find-completions-at
             #"("
             (make-position line 0 character 1)
             (trigger-kind-character))))
