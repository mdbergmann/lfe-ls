(defmodule lsp-util
  (export (completion-item-to-json 1)))

(include-lib "apps/lfe-ls/include/lsp-model.lfe")

(defun completion-item-to-json (citem)
  "Converts `completion-item` to LSP json representation."
  (let ((module (completion-item-module citem))
        (func (completion-item-func citem))
        (arity (completion-item-arity citem))
        (detail (completion-item-detail citem))
        (kind (completion-item-kind citem)))
    (let* ((base-label (case func
                         ('null (lfe_io:format1 "~s" `(,module)))
                         (#"" (lfe_io:format1 "~s" `(,module)))
                         (fn (lfe_io:format1 "~s:~s" `(,module ,fn)))))
           (label (list_to_binary
                   (case arity
                     ('null base-label)
                     (ar (lfe_io:format1 "~s~s"
                                 `(,base-label
                                   ,(lfe_io:format1 "/~p" `(,ar))))))))
           (insertText (case func
                         ('null module)
                         (#"" module)
                         (fn (case arity
                               ('null fn)
                               (0 fn)
                               (n (list_to_binary
                                   (lists:foldl (lambda (i acc)
                                                  (lfe_io:format1 "~s ${~p:arg~p}" `(,acc ,i ,i)))
                                          fn
                                          (lists:seq 1 n))))))))
           (result `(#(#"label" ,label)
                     #(#"kind" ,kind)
                     #(#"detail" ,detail)
                     #(#"insertTextFormat" 2)
                     #(#"insertText" ,insertText))))
      ;;(logger:notice "result: ~p" `(,result))
      result)))

