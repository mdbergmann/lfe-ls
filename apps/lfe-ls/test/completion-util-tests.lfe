(defmodule completion-util-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "apps/lfe-ls/include/lsp-model.lfe")

(deftest find-completions--trigger-character--open-paren--global-symbols
  (let ((funs (lists:sort (completion-util:find-completions-at
                           (make-position line 0 character 1)
                           #"("
                           #"("))))
    ;;(logger:notice "~p" `(,funs))
    (is-equal `#(completion-item #"application" #"" null #"" 9)
              (car funs))
    (is-equal `#(completion-item #"zlib" #"" null #"" 9)
              (car (lists:reverse funs)))
    (is-equal `#(completion-item #"lfe-core" #"binary" null #"" 14)
              (cl:elt 1 (lists:search (lambda (item) (== #"binary" (completion-item-func item)))
                                      funs)))))

(deftest find-completions--trigger-character--colon--module-functions--paren-delim
  (let ((funs (lists:sort (completion-util:find-completions-at
                           (make-position line 0 character 4)
                           #"(io:"
                           #":"))))
    ;;(logger:notice "~p" `(,funs))
    (is-equal `#(completion-item #"io" #"columns" 0 #"" 3)
              (car funs))
    (is-equal `#(completion-item #"io" #"write" 2 #"" 3)
              (car (lists:reverse funs)))))

(deftest find-completions--trigger-character--colon--module-functions--space-delim
  (let ((funs (lists:sort (completion-util:find-completions-at
                           (make-position line 0 character 4)
                           #" io:"
                           #":"))))
    (is-equal `#(completion-item #"io" #"columns" 0 #"" 3)
              (car funs))
    (is-equal `#(completion-item #"io" #"write" 2 #"" 3)
              (car (lists:reverse funs)))))

(deftest find-completions--trigger-character--colon--no-delim
  (let ((funs (lists:sort (completion-util:find-completions-at
                           (make-position line 0 character 4)
                           #"io:"
                           #":"))))
    (is-equal `#(completion-item #"io" #"columns" 0 #"" 3)
              (car funs))
    (is-equal `#(completion-item #"io" #"write" 2 #"" 3)
              (car (lists:reverse funs)))))

(deftest find-completions--invoked--symbol-or-module--space-delim
  (let ((funs (lists:sort (completion-util:find-completions-at
                           (make-position line 0 character 3)
                           #" de"
                           'null))))
    (is-equal `#(completion-item #"application" #"" null #"" 9)
              (car funs))
    (is-equal `#(completion-item #"zlib" #"" null #"" 9)
              (car (lists:reverse funs)))))

(deftest find-completions--invoked--symbol-or-module--no-delim
  (let ((funs (lists:sort (completion-util:find-completions-at
                           (make-position line 0 character 2)
                           #"de"
                           'null))))
    (is-equal `#(completion-item #"application" #"" null #"" 9)
              (car funs))
    (is-equal `#(completion-item #"zlib" #"" null #"" 9)
              (car (lists:reverse funs)))))

(deftest find-completions--invoked--symbol-or-module--paren-delim
  (let ((funs (lists:sort (completion-util:find-completions-at
                           (make-position line 0 character 3)
                           #"(de"
                           'null))))
    (is-equal `#(completion-item #"application" #"" null #"" 9)
              (car funs))
    (is-equal `#(completion-item #"zlib" #"" null #"" 9)
              (car (lists:reverse funs)))))

(deftest find-completions--invoked--module-functions--paren-delim--module
  (let ((funs (lists:sort (completion-util:find-completions-at
                           (make-position line 0 character 5)
                           #"(io:fo"
                           'null))))
    (is-equal `#(completion-item #"io" #"columns" 0 #"" 3)
              (car funs))
    (is-equal `#(completion-item #"io" #"write" 2 #"" 3)
              (car (lists:reverse funs)))))

(deftest find-completions--invoked--module-functions--space-delim--module
  (let ((funs (lists:sort (completion-util:find-completions-at
                           (make-position line 0 character 5)
                           #" io:fo"
                           'null))))
    (is-equal `#(completion-item #"io" #"columns" 0 #"" 3)
              (car funs))
    (is-equal `#(completion-item #"io" #"write" 2 #"" 3)
              (car (lists:reverse funs)))))

(deftest find-completions--invoked--module-functions--no-delim--module
  (let ((funs (lists:sort (completion-util:find-completions-at
                           (make-position line 0 character 5)
                           #"io:fo"
                           'null))))
    (is-equal `#(completion-item #"io" #"columns" 0 #"" 3)
              (car funs))
    (is-equal `#(completion-item #"io" #"write" 2 #"" 3)
              (car (lists:reverse funs)))))

(deftest find-completions--invoked--module-functions--no-delim--module-2
  (let ((funs (lists:sort (completion-util:find-completions-at
                           (make-position line 0 character 3)
                           #"io:"
                           'null))))
    (is-equal `#(completion-item #"io" #"columns" 0 #"" 3)
              (car funs))
    (is-equal `#(completion-item #"io" #"write" 2 #"" 3)
              (car (lists:reverse funs)))))

(deftest find-completions--invoked--module-functions--paren-delim--module-3
  (let ((funs (lists:sort (completion-util:find-completions-at
                           (make-position line 1 character 6)
                           #"(defun foo (text)
  (io:))"
                           'null))))
    (is-equal `#(completion-item #"io" #"columns" 0 #"" 3)
              (car funs))
    (is-equal `#(completion-item #"io" #"write" 2 #"" 3)
              (car (lists:reverse funs)))))

(deftest find-completions--invoked--module-functions--no-delim--module--char-pos-out
  (let ((funs (lists:sort (completion-util:find-completions-at
                           (make-position line 0 character 1)
                           #"io:"
                           'null))))
    (is-equal `#(completion-item #"application" #"" null #"" 9)
              (car funs))
    (is-equal `#(completion-item #"zlib" #"" null #"" 9)
              (car (lists:reverse funs)))))

(deftest find-completions--empty-line
  (let ((funs (lists:sort (completion-util:find-completions-at
                           (make-position line 1 character 4)
                           #""
                           'null))))
    (is-equal `#(completion-item #"application" #"" null #"" 9)
              (car funs))
    (is-equal `#(completion-item #"zlib" #"" null #"" 9)
              (car (lists:reverse funs)))))
