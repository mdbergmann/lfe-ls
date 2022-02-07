(defmodule lsp-proc
  (behaviour gen_server)
  (export
   (start 1)
   (init 1)
   (handle_call 3)
   (handle_cast 2)
   (handle_info 2)
   (terminate 2))
  (import (from lfe-ls
                (concat-binary 2))))

(defrecord proc-state (parent-pid))

;; ---------------------------
;; processor
;; ---------------------------

(defun %req-parse-error () -32700)
(defun %req-invalid-request-error () -32600)

(defun %process-input (input)
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

;; -----------------------
;; Server
;; -----------------------

(defun start (parent-pid)
  (gen_server:start 'lsp-proc parent-pid '()))

(defun init (parent-pid)
  (logger:debug "init (lsp-proc)")
  `#(ok (make-proc-state parent-pid parent-pid)))

(defun handle_call (arg from state)
  (logger:debug "handle_call: ~p" `(,arg))
  (case arg
    (`#(process-input ,input)
     `#(reply ,(%process-input input) state))))

(defun handle_cast
  (('stop state)
   (logger:info "stopping...")
   `#(stop normal ,state))
  ((_ state)
   `#(noreply ,state)))

(defun handle_info
  ((`#(EXIT pid reason) state)
   (logger:info "Request to exit!")
   `#(noreply ,state)))

(defun terminate
  (('shutdown _state)
   'ok))
