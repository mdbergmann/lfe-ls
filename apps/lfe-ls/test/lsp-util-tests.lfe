(defmodule lsp-util-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "apps/lfe-ls/include/lsp-model.lfe")

(deftest generate-json--arity-null
  (is-equal '(#(#"label" #"foo:bar")
              #(#"kind" 3)
              #(#"detail" #"")
              #(#"insertTextFormat" 2)
              #(#"insertText" #"bar"))
            (lsp-util:completion-item-to-json
                             (make-completion-item
                              module #"foo"
                              func #"bar"
                              arity 'null
                              kind (completion-item-kind-function)))))

(defmacro make-citem (arity)
  `(lsp-util:completion-item-to-json
                    (make-completion-item
                     module #"foo"
                     func #"bar"
                     arity ,arity
                     kind (completion-item-kind-function))))

(deftest generate-json--arity-0
  (is-equal '(#(#"label" #"foo:bar/0")
              #(#"kind" 3)
              #(#"detail" #"")
              #(#"insertTextFormat" 2)
              #(#"insertText" #"bar"))
            (make-citem 0)))

(deftest generate-json--arity-1
  (is-equal '(#(#"label" #"foo:bar/1")
              #(#"kind" 3)
              #(#"detail" #"")
              #(#"insertTextFormat" 2)
              #(#"insertText" #"bar ${1:arg1}"))
            (make-citem 1)))

(deftest generate-json--arity-2
  (is-equal '(#(#"label" #"foo:bar/2")
              #(#"kind" 3)
              #(#"detail" #"")
              #(#"insertTextFormat" 2)
              #(#"insertText" #"bar ${1:arg1} ${2:arg2}"))
            (make-citem 2)))

(deftest generate-json--arity-3
  (is-equal '(#(#"label" #"foo:bar/3")
              #(#"kind" 3)
              #(#"detail" #"")
              #(#"insertTextFormat" 2)
              #(#"insertText" #"bar ${1:arg1} ${2:arg2} ${3:arg3}"))
            (make-citem 3)))
