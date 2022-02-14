(defmodule completion-util-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "apps/lfe-ls/include/lsp-model.lfe")

(deftest find-completions--trigger-character--open-paren--global-symbols
  (let ((funs (completion-util:find-completions-at
               #"("
               (make-position line 0 character 1)
               #"(")))
    (is-equal `#(completion-item #"'macro-function'/1" 3 #"lfe:")
              (car funs))
    (is-equal `#(completion-item #"whereis/1" 3 #"erlang:")
              (car (lists:reverse funs)))
    ;;(logger:notice "funs: ~p" `(,funs))
    ))
