(defmodule lsp-proc
  (export (process-input 1)))

(defun process-input (input)
  (try
      (let ((json-input (ljson:decode input)))
        (case json-input
          (`(#(#"jsonrpc" #"2.0")
             #(#"id" ,req-id)
             #(#"method" ,req-method)
             #(#"params" ,req-params))
           (process-method req-id req-method req-params))
          (_ `#(warn "Unrecognized request!"))))
    (catch
      ((tuple type value stacktrace)
       (progn
         (logger:error "Error on json operation: ~p, type: ~p, value: ~p"
                       `(,stacktrace ,type ,value))
         `#(error "Unable to decode json!"))))))

(defun process-method (id method params)
  `#(ok ,(ljson:encode `(#(#"id" ,id) #(#"result" true)))))
