(defmodule completion-util-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "apps/lfe-ls/include/lsp-model.lfe")

(deftest find-completions--trigger-character--open-paren--global-symbols
  (let ((funs (lists:sort (completion-util:find-completions-at
                           #"("
                           (make-position line 0 character 1)
                           #"("))))
    (is-equal `#(completion-item #"'macro-function'/1" 3 #"lfe:")
              (car funs))
    (is-equal `#(completion-item #"zlib" 9 #"")
              (car (lists:reverse funs)))))

(deftest find-completions--trigger-character--colon--module-functions--paren-delim
  (let ((funs (completion-util:find-completions-at
               #"(io:"
               (make-position line 0 character 4)
               #":")))
    (is-equal `#(completion-item #"columns/0" 3 #"io:")
              (car funs))
    (is-equal `#(completion-item #"write/2" 3 #"io:")
              (car (lists:reverse funs)))))

(deftest find-completions--trigger-character--colon--module-functions--space-delim
  (let ((funs (completion-util:find-completions-at
               #" io:"
               (make-position line 0 character 4)
               #":")))
    (is-equal `#(completion-item #"columns/0" 3 #"io:")
              (car funs))
    (is-equal `#(completion-item #"write/2" 3 #"io:")
              (car (lists:reverse funs)))))

(deftest find-completions--trigger-character--colon--no-delim
  (let ((funs (completion-util:find-completions-at
               #"io:"
               (make-position line 0 character 4)
               #":")))
    (is-equal `#(completion-item #"columns/0" 3 #"io:")
              (car funs))
    (is-equal `#(completion-item #"write/2" 3 #"io:")
              (car (lists:reverse funs)))))

(deftest find-completions--invoked--symbol-or-module--space-delim
  (let ((funs (completion-util:find-completions-at
               #" de"
               (make-position line 0 character 3)
               'null)))
    (is-equal `#(completion-item #"'macro-function'/1" 3 #"lfe:")
              (car funs))
    (is-equal `#(completion-item #"zlib" 9 #"")
              (car (lists:reverse funs)))))

(deftest find-completions--invoked--symbol-or-module--no-delim
  (let ((funs (completion-util:find-completions-at
               #"de"
               (make-position line 0 character 2)
               'null)))
    (is-equal `#(completion-item #"'macro-function'/1" 3 #"lfe:")
              (car funs))
    (is-equal `#(completion-item #"zlib" 9 #"")
              (car (lists:reverse funs)))))

(deftest find-completions--invoked--symbol-or-module--paren-delim
  (let ((funs (completion-util:find-completions-at
               #"(de"
               (make-position line 0 character 3)
               'null)))
    (is-equal `#(completion-item #"'macro-function'/1" 3 #"lfe:")
              (car funs))
    (is-equal `#(completion-item #"zlib" 9 #"")
              (car (lists:reverse funs)))))

(deftest find-completions--invoked--module-functions--paren-delim--module
  (let ((funs (completion-util:find-completions-at
               #"(io:fo"
               (make-position line 0 character 5)
               'null)))
    (is-equal `#(completion-item #"columns/0" 3 #"io:")
              (car funs))
    (is-equal `#(completion-item #"write/2" 3 #"io:")
              (car (lists:reverse funs)))))

(deftest find-completions--invoked--module-functions--space-delim--module
  (let ((funs (completion-util:find-completions-at
               #" io:fo"
               (make-position line 0 character 5)
               'null)))
    (is-equal `#(completion-item #"columns/0" 3 #"io:")
              (car funs))
    (is-equal `#(completion-item #"write/2" 3 #"io:")
              (car (lists:reverse funs)))))

(deftest find-completions--invoked--module-functions--no-delim--module
  (let ((funs (completion-util:find-completions-at
               #"io:fo"
               (make-position line 0 character 5)
               'null)))
    (is-equal `#(completion-item #"columns/0" 3 #"io:")
              (car funs))
    (is-equal `#(completion-item #"write/2" 3 #"io:")
              (car (lists:reverse funs)))))

(deftest find-completions--invoked--module-functions--no-delim--module-2
  (let ((funs (completion-util:find-completions-at
               #"io:"
               (make-position line 0 character 3)
               'null)))
    (is-equal `#(completion-item #"columns/0" 3 #"io:")
              (car funs))
    (is-equal `#(completion-item #"write/2" 3 #"io:")
              (car (lists:reverse funs)))))

(deftest find-completions--invoked--module-functions--paren-delim--module-3
  (let ((funs (completion-util:find-completions-at
               #"(defun foo (text)
  (io:))"
               (make-position line 1 character 6)
               'null)))
    (is-equal `#(completion-item #"columns/0" 3 #"io:")
              (car funs))
    (is-equal `#(completion-item #"write/2" 3 #"io:")
              (car (lists:reverse funs)))))

(deftest find-completions--invoked--module-functions--no-delim--module--char-pos-out
  (let ((funs (completion-util:find-completions-at
               #"io:"
               (make-position line 0 character 1)
               'null)))
    ;;(logger:notice "funs: ~p" `(,funs))
    (is-equal `#(completion-item #"'macro-function'/1" 3 #"lfe:")
              (car funs))
    (is-equal `#(completion-item #"zlib" 9 #"")
              (car (lists:reverse funs)))))

(deftest find-completions--empty-line
  (let ((funs (completion-util:find-completions-at
               #""
               (make-position line 1 character 4)
               'null)))
    (is-equal `#(completion-item #"'macro-function'/1" 3 #"lfe:")
              (car funs))
    (is-equal `#(completion-item #"zlib" 9 #"")
              (car (lists:reverse funs)))))
