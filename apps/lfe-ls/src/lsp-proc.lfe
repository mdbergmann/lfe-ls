(defmodule lsp-proc
  (export
   (process-input 2)))

(include-lib "apps/lfe-ls/include/utils.lfe")
(include-lib "apps/lfe-ls/include/lsp-model.lfe")

(defun %req-parse-error () -32700)
(defun %req-invalid-request-error () -32600)

(defun process-input (input state)
  "Main interface function.
Takes string `input' (the request json-rpc payload) and produces
output, the json-rpc response, as string.
Conversion to and from json<->string is done in this function.
The `state' argument is used an instance of `lsp-state' record that represents the current
LSP server state.

This function returns:
`(tuple (tuple code response) state)`
where `code' is `reply' or `noreply'. `response' is the json-rpc respose payload.
`state' is the new, if changed, or old LSP server state."
  (case (try
            (let ((json-input (ljson:decode input)))
              ;;(logger:debug "json-input: ~p" `(,json-input))
              (case json-input
                (`(#(#"jsonrpc" #"2.0")
                   #(#"id" ,req-id)
                   #(#"method" ,req-method)
                   #(#"params" ,req-params))
                 (%process-method req-id req-method req-params state))
                (`(#(#"jsonrpc" #"2.0")
                   #(#"method" ,req-method)
                   #(#"params" ,req-params))
                 (%process-method 'null req-method req-params state))
                (_
                 (logger:warning "Invalid lsp header!")
                 `#(#(reply ,(%make-error-response 'null
                                                   (%req-invalid-request-error)
                                                   #"Invalid LSP header!"))
                    ,state))))
          (catch
            ((tuple type value stacktrace)
             (progn
               (logger:warning "Error on json operation: ~p, type: ~p, value: ~p"
                               `(,stacktrace ,type ,value))
               `#(#(reply ,(%make-error-response 'null
                                                 (%req-parse-error)
                                                 #"Error on parsing json!"))
                  ,state)))))
    (`#(#(,code ,response) ,state)
     `#(#(,code ,(ljson:encode response)) ,state))))

(defun %process-method (id method params state)
  "This function is the main lsp 'method' dispatcher.

It returns:
`(tuple (tuple code response) new-state)`
where `code' is either `reply' or `noreply' indicating that the response has to be sent back to the requester or not. LSP notifications don't require reply but requests do.
`response' is the generated lsp response for the received request.
`new-state' is for state changes that need to be transported back to the state keeper."
  (case method
    (#"initialize"
     (%on-initialize-req id params state))
    (#"initialized"
     `#(,(%on-initialized-req id params) ,state))
    (#"textDocument/didOpen"
     (%on-textDocument/didOpen-req id params state))
    (#"textDocument/didClose"
     (%on-textDocument/didClose-req id params state))
    (#"textDocument/didChange"
     (%on-textDocument/didChange-req id params state))
    (#"textDocument/completion"
     (%on-textDocument/completion-req id params state))
    (#"test-success"
     `#(#(reply ,(%make-result-response id 'true)) ,state))
    (_
     `#(#(noreply ,(%make-error-response id
                                         (%req-invalid-request-error)
                                         (concat-binary #"Method not supported: '"
                                                        (concat-binary method #"'!"))))
        ,state))))

;; method handlers

(defun %on-initialize-req (id params state)
  `#(#(reply ,(%make-result-response id (%make-initialize-result params)))
     ,(set-lsp-state-initialized state 'true)))

(defun %on-initialized-req (id params)
  `#(noreply null))

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
  "Text sync is full dopcument."
  #(#"capabilities" (#(#"completionProvider"
                       (#(#"resolveProvider" true)
                        #(#"triggerCharacters" (#"(" #":" #"'"))))
                     #(#"textDocumentSync"
                       (#(#"openClose" true) #(#"change" 1))))))

(defun %make-completion-result (completions)
  "`completions' is a list of `completion-item' records."
  (lists:map (lambda (citem)
               `(#(#"label" ,(completion-item-label citem))
                 #(#"kind" ,(completion-item-kind citem))
                 #(#"detail" ,(completion-item-detail citem))))
             completions))
