(defmodule lsp-proc
  (export
   (process-input 3)))

(include-lib "apps/lfe-ls/include/utils.lfe")
(include-lib "apps/lfe-ls/include/lsp-model.lfe")

(defun process-input (input state send-fun)
  "Main interface function.
Takes string `input' (the request json-rpc payload) and produces
output, the json-rpc response, as string.
Conversion to and from json<->string is done in this function.
The `state' argument is used an instance of `lsp-state' record that represents the current
LSP server state.

This function returns a newly computed state for the caller."
  (try
      (let ((json-input (ljson:decode input)))
        (logger:debug "json-input: ~p" `(,json-input))
        (let* ((`#(#"jsonrpc" #"2.0") (find-tkey #"jsonrpc" json-input))
               (`#(#"method" ,req-method) (find-tkey #"method" json-input))
               (`#(#"params" ,req-params) (find-tkey #"params" json-input))
               (tmp-req-id (find-tkey #"id" json-input))
               (req-id (case tmp-req-id
                         ('() 'null)
                         (id (tcdr id)))))
          (%process-method req-id req-method req-params state send-fun)))
    (catch
      ((tuple type value stacktrace)
       (progn
         (logger:warning "Error: ~p, type: ~p, value: ~p"
                         `(,stacktrace ,type ,value))
         (funcall send-fun `#(reply ,(ljson:encode
                                      (%make-error-response
                                       'null
                                       (req-parse-error)
                                       #"Error on handling request!"))))
         state)))))

(defun %process-method (id method params state send-fun)
  "This function is the main lsp 'method' dispatcher.
It returns a new state.
The response, either synchronous or asynchronous as notification is done via `pid`
which requires a `(tuple code response)` tuple where:
`code`: is `noreply`, `reply` or `notify`.
`response`: is the LSP JSON response"
  (logger:info "processing method: ~p" `(,method))
  (case (case method
          (#"initialize"
           (%on-initialize-req id params state))
          (#"initialized"
           (%on-initialized-req id params state))
          (#"textDocument/didOpen"
           (%on-textDocument/didOpen-req id params state))
          (#"textDocument/didClose"
           (%on-textDocument/didClose-req id params state))
          (#"textDocument/didChange"
           (%on-textDocument/didChange-req id params state))
          (#"textDocument/didSave"
           (%on-textDocument/didSave-req id params state))
          (#"textDocument/completion"
           (%on-textDocument/completion-req id params state))
          (#"shutdown"
           (%on-shutdown-req id state))
          (#"test-success"
           `#(#(reply
                ,(%make-result-response id 'true)) ,state))
          (_
           `#(#(noreply
                ,(%make-error-response
                  id
                  (req-invalid-request-error)
                  (concat-binary #"Method not supported: '"
                                 (concat-binary method #"'!"))))
              ,state)))
    ((tuple (tuple code response) state)
     (funcall send-fun `#(,code ,(ljson:encode response)))
     state)))

;; method handlers

(defun %on-initialize-req (id params state)
  `#(#(reply ,(%make-result-response id (%make-initialize-result params)))
     ,(set-lsp-state-initialized state 'true)))

(defun %on-initialized-req (id params state)
  `#(#(noreply null) ,state))

(defun %on-textDocument/didOpen-req (id params state)
  (let ((state-documents (lsp-state-documents state)))
    (case params
      (`(#(#"textDocument" ,text-document))
       (let ((`#(#"uri" ,uri) (find-tkey #"uri" text-document))
             (`#(#"version" ,version) (find-tkey #"version" text-document))
             (`#(#"text" ,text) (find-tkey #"text" text-document)))
         `#(#(noreply null)
            ,(set-lsp-state-documents
              state
              (map-set state-documents
                       uri
                       (make-document uri uri version version text text))))))
      (_
       (logger:warning "Missing 'textDocument' param!")
       `#(#(noreply null) ,state)))))

(defun %on-textDocument/didClose-req (id params state)
  (let ((state-documents (lsp-state-documents state)))
    (case params
      (`(#(#"textDocument" ,text-document))
       (let ((`#(#"uri" ,uri) (find-tkey #"uri" text-document)))
         `#(#(noreply null)
            ,(set-lsp-state-documents
              state
              (map-remove state-documents uri)))))
      (_
       (logger:warning "Missing 'textDocument' param!")
       `#(#(noreply null) ,state)))))

(defun %on-textDocument/didChange-req (id params state)
  (let ((`#(#"textDocument" ,text-document) (find-tkey #"textDocument" params))
        (`#(#"contentChanges" ,content-changes) (find-tkey #"contentChanges" params)))
    (let ((`#(#"uri" ,uri) (find-tkey #"uri" text-document))
          (`#(#"version" ,version) (find-tkey #"version" text-document)))
      (let* ((state-documents (lsp-state-documents state))
             (document (map-get state-documents uri))
             (current-text (document-text document))
             (new-text (cl:reduce (lambda (change acc)
                                    (let ((`#(#"text" ,updated-text) (find-tkey #"text" change)))
                                      updated-text))
                                  content-changes
                                  'initial-value current-text)))
        `#(#(noreply null)
           ,(set-lsp-state-documents
             state
             (map-set state-documents
                      uri
                      (clj:-> document
                              (set-document-text new-text)
                              (set-document-version version)))))))))

(defun %on-textDocument/didSave-req (id params state)
  (let ((`#(#"textDocument" ,text-document) (find-tkey #"textDocument" params)))
    (let ((`#(#"uri" ,uri) (find-tkey #"uri" text-document)))
      (let* ((state-documents (lsp-state-documents state))
             (document (map-get state-documents uri))
             (version (document-version document)))
        `#(#(notify ,(%make-notification
                      #"textDocument/publishDiagnostics"
                      (%make-diagnostic-params
                       uri version
                       (compile-util:compile-file uri)))) ,state)))))

(defun %on-textDocument/completion-req (id params state)
  (let ((`#(#"textDocument" ,text-document) (find-tkey #"textDocument" params))
        (`#(#"position" ,position) (find-tkey #"position" params))
        (`#(#"context" ,context) (find-tkey #"context" params)))
    (let ((`(#(#"uri" ,uri)) text-document)
          (`(#(#"line" ,line) #(#"character" ,character)) position)
          (trigger-char (find-tkey #"triggerCharacter" context)))
      (let* ((state-documents (lsp-state-documents state))
             (document (map-get state-documents uri))
             (text (document-text document)))
        `#(#(reply ,(%make-result-response
                     id
                     (%make-completion-result
                      (completion-util:find-completions-at
                       text
                       (make-position line line
                                      character character)
                       (case trigger-char
                         ('() 'null)
                         (else (tcdr else)))))))
           ,state)))))

(defun %on-shutdown-req (id state)
  `#(#(reply ,(%make-result-response id 'null)) ,state))

;; response factories

(defun %make-result-response (id result)
  `(#(#"id" ,id) #(#"result" ,result)))

(defun %make-error-response (id code err-msg)
  `(#(#"id" ,id) #(#"error" (#(#"code" ,code)
                             #(#"message" ,err-msg)))))

(defun %make-initialize-result (req-params)
  `(,(%make-capabilities)
    #(#"serverInfo" (#(#"name" #"lfe-ls")))))

(defun %make-capabilities ()
  "Text sync is full document."
  #(#"capabilities" (#(#"completionProvider"
                       (#(#"resolveProvider" true)
                        #(#"triggerCharacters" (#"(" #":" #"'"))))
                     #(#"textDocumentSync"
                       (#(#"openClose" true) #(#"change" 1))))))

(defun %make-completion-result (completions)
  "`completions' is a list of `completion-item' records."
  (lists:map (lambda (citem)
               (let ((insert-text (completion-item-insert-text citem)))
                 (lists:append
                  `(#(#"label" ,(completion-item-label citem))
                    #(#"kind" ,(completion-item-kind citem))
                    #(#"detail" ,(completion-item-detail citem)))
                  (if (> (byte_size insert-text) 0)
                    `(#(#"insertText" ,insert-text))
                    '()))))
             completions))

(defun %make-notification (method params)
  `(#(#"jsonrpc" #"2.0")
    #(#"method" ,method)
    #(#"params" ,params)))

(defun %make-diagnostic-params (uri version diagnostics)
  "Diagnostics are a list diagnostic records."
  `(#(#"uri" ,uri)
    #(#"version" ,version)
    #(#"diagnostics" ())))
