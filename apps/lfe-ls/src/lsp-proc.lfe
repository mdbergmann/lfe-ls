(defmodule lsp-proc
  (export (process-input 1)))

(defun process-input (input)
  (try
      (let ((json-input (ljson:decode input)))
        `#(ok ,(ljson:encode '(#(#"id" 99)
                               #(#"result" true)))))
    (catch
      ((tuple type value stacktrace)
       (progn
         (logger:error "Error on json operation: ~p, type: ~p, value: ~p"
                       `(,stacktrace ,type ,value))
         `#(error "Unable to decode json!"))))))
