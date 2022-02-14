(defmodule completion-util-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "apps/lfe-ls/include/lsp-model.lfe")

(deftest find-completions--trigger-character--open-paren--global-symbols
  (let ((funs (completion-util:find-completions-at
               #"("
               (make-position line 0 character 1)
               #"(")))
    ;;(logger:notice "funs: ~p" `(,funs))
    (is-equal `#(completion-item #"'macro-function'/1" 3 #"lfe:")
              (car funs))
    (is-equal `#(completion-item #"whereis/1" 3 #"erlang:")
              (car (lists:reverse funs)))
    ))

(deftest find-completions--trigger-character--colon--module-functions
  (let ((funs (completion-util:find-completions-at
               #"(io:"
               (make-position line 0 character 4)
               #":")))
    ;;(logger:notice "funs: ~p" `(,funs))
    (is-equal `#(completion-item #"columns/0" 3 #"io:")
              (car funs))
    (is-equal `#(completion-item #"write/2" 3 #"io:")
              (car (lists:reverse funs)))
    ))
