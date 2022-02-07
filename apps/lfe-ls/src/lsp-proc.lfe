(defmodule lsp-proc
  (export (process-input 1)))

(defun req-parse-error () -32700)

(defun process-input (input)
  (case (try
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
               `#(ok (#(#"id" null) #(#"error" (#(#"code" ,(req-parse-error))
                                                #(#"message" #"Error on parsing json!")))))))))
    (`#(ok ,response) `#(ok ,(ljson:encode response)))))

(defun process-method (id method params)
  `#(ok ,(case method
           (#"initialize"
            `(#(#"id" ,id) #(#"result" ,(make-initialize-result params))))
           (_
            `(#(#"id" ,id) #(#"result" true))))))

(defun make-initialize-result (req-params)
  '(#(#"capabilities" #())
    #(#"serverInfo" (#(#"name" #"lfe-ls")))))
