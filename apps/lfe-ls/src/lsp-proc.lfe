(defmodule lsp-proc
  (export
   (process-input 1)))

(include-lib "apps/lfe-ls/include/utils.lfe")
(include-lib "apps/lfe-ls/include/lsp-model.lfe")

(defun %req-parse-error () -32700)
(defun %req-invalid-request-error () -32600)

(defun process-input (input)
  (case (try
            (let ((json-input (ljson:decode input)))
              (case json-input
                (`(#(#"jsonrpc" #"2.0")
                   #(#"id" ,req-id)
                   #(#"method" ,req-method)
                   #(#"params" ,req-params))
                 (%process-method req-id req-method req-params))
                (_ `#(warn "Unrecognized request!"))))
          (catch
            ((tuple type value stacktrace)
             (progn
               (logger:warning "Error on json operation: ~p, type: ~p, value: ~p"
                               `(,stacktrace ,type ,value))
               `#(ok ,(%make-error-response 'null
                                           (%req-parse-error)
                                           #"Error on parsing json!"))))))
    (`#(ok ,response) `#(ok ,(ljson:encode response)))))

(defun %process-method (id method params)
  `#(ok ,(case method
           (#"initialize"
            (%make-result-response id (%make-initialize-result params)))
           (#"test-success"
            (%make-result-response id 'true))
           (_
            (%make-error-response id
                                 (%req-invalid-request-error)
                                 (concat-binary #"Method not supported: '"
                                                (concat-binary method #"'!")))))))

(defun %make-result-response (id result)
  `(#(#"id" ,id) #(#"result" ,result)))

(defun %make-error-response (id code err-msg)
  `(#(#"id" ,id) #(#"error" (#(#"code" ,code)
                             #(#"message" ,err-msg)))))

(defun %make-initialize-result (req-params)
  '(#(#"capabilities" #())
    #(#"serverInfo" (#(#"name" #"lfe-ls")))))
