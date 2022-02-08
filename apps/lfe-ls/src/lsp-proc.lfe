(defmodule lsp-proc
  (export
   (process-input 2)))

(include-lib "apps/lfe-ls/include/utils.lfe")
(include-lib "apps/lfe-ls/include/lsp-model.lfe")

(defun %req-parse-error () -32700)
(defun %req-invalid-request-error () -32600)

(defun process-input (input state)
  (case (try
            (let ((json-input (ljson:decode input)))
              (case json-input
                (`(#(#"jsonrpc" #"2.0")
                   #(#"id" ,req-id)
                   #(#"method" ,req-method)
                   #(#"params" ,req-params))
                 (%process-method req-id req-method req-params state))
                (_ `#(warn "Unrecognized request!"))))
          (catch
            ((tuple type value stacktrace)
             (progn
               (logger:warning "Error on json operation: ~p, type: ~p, value: ~p"
                               `(,stacktrace ,type ,value))
               `#(ok ,(%make-error-response 'null
                                            (%req-parse-error)
                                            #"Error on parsing json!")
                     ,state)))))
    (`#(ok ,response ,state) `#(ok ,(ljson:encode response) ,state))))

(defun %process-method (id method params state)
  (let ((`#(,response ,new-state)
         (case method
           (#"initialize"
            (case (%on-initialize id params)
              (`#(ok ,response)
               `#(,response ,(set-lsp-state-initialized state 'true)))))
           (#"test-success"
            `#(,(%make-result-response id 'true) ,state))
           (_
            `#(,(%make-error-response id
                                      (%req-invalid-request-error)
                                      (concat-binary #"Method not supported: '"
                                                     (concat-binary method #"'!")))
               ,state)))))
    `#(ok ,response ,new-state)))

(defun %on-initialize (id params)
  `#(ok ,(%make-result-response id (%make-initialize-result params))))

(defun %make-result-response (id result)
  `(#(#"id" ,id) #(#"result" ,result)))

(defun %make-error-response (id code err-msg)
  `(#(#"id" ,id) #(#"error" (#(#"code" ,code)
                             #(#"message" ,err-msg)))))

(defun %make-initialize-result (req-params)
  `(,(%make-capabilities)
    #(#"serverInfo" (#(#"name" #"lfe-ls")))))

(defun %make-capabilities ()
  #(#"capabilities" (#(#"textDocument"
                       (#(#"completion"
                          (#(#"dynamicRegistration" false))))))))
