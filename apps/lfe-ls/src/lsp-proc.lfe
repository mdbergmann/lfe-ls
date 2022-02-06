(defmodule lsp-proc
  (export (process-input 1)))

(defun process-input (input)
  (try
      (let ((json-input (ljson:decode input)))
        `#(ok ,(ljson:encode #(#"Bar" #"foo"))))
    (catch
      ((tuple type value stacktrace)
       (logger:error "Error on json operation: ~p" `(,stacktrace))
       `#(error "Unable to decode json!")))))
