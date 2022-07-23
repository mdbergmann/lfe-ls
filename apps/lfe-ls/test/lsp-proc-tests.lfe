(defmodule lsp-proc-tests
  (behaviour ltest-unit)
  (export (fake-lsp-resp-sender 1)))

(include-lib "ltest/include/ltest-macros.lfe")

(include-lib "apps/lfe-ls/include/utils.lfe")
(include-lib "apps/lfe-ls/include/lsp-model.lfe")

(defun fake-lsp-resp-sender (lsp-resp)
  (receive
    ((tuple from 'get)
     (! from `#(get-resp ,lsp-resp))
     (fake-lsp-resp-sender lsp-resp))
    ((tuple from 'put new-response)
     (fake-lsp-resp-sender new-response))
    ('terminate
     'ok)))

(defmacro with-fixture body
  `(let ((receiver (spawn (MODULE) 'fake-lsp-resp-sender '(()))))
     ,@body
    (! receiver 'terminate)))


;; (deftest error-on-decoding
;;   (is-equal `#(#(reply
;;                  #"{\"id\":null,\"error\":{\"code\":-32700,\"message\":\"Error on parsing json!\"}}")
;;                ,(make-lsp-state))
;;             (lsp-proc:process-input #"{\"Foo\"}" (make-lsp-state))))

(defmacro expected-result-p (expected-result)
  "Probes `receiver` for result."
  `(assert-cond (lambda ()
                  (! receiver `#(,(self) get))
                  (receive
                    ((tuple 'get-resp lsp-resp)
                     (cond
                      ((?= ,expected-result
                           lsp-resp)
                       'true)
                      (else 'false)))
                    (after 500
                      'false)))
                1000))

(defmacro fake-sender-fun ()
  "lsp-proc will call this lambda.
This one will just push the computed result to our fake-lsp-resp-sender actor"
  `(let ((me (self)))
     (lambda (lsp-resp)
       (! receiver `#(,me put ,lsp-resp)))))

(deftest test-lsp-receiver
  (with-fixture
   (! receiver `#(,(self) get))
   (receive
     ((tuple 'get-resp lsp-resp)
      (is-equal '() lsp-resp))
     (after 500
       (is 'false)))
   (! receiver `#(,(self) put 0))
   (! receiver `#(,(self) get))
   (receive
     ((tuple 'get-resp lsp-resp)
      (is-equal 0 lsp-resp))
     (after 500
       (is 'false)))))

(deftest error-invalid-request--method-not-implemented
  (with-fixture
   (is-equal (make-lsp-state)
             (lsp-proc:process-input #"{
\"jsonrpc\":\"2.0\",
\"method\":\"not-supported\",
\"id\":99,
\"params\":{}
}"
                                     (make-lsp-state)
                                     (fake-sender-fun)))
   (is (expected-result-p
        #(noreply
          #"{\"id\":99,\"error\":{\"code\":-32600,\"message\":\"Method not supported: 'not-supported'!\"}}")))))

(deftest process-simple-message
  (with-fixture
   (is-equal (make-lsp-state)
             (lsp-proc:process-input #"{
\"jsonrpc\":\"2.0\",
\"id\":99,
\"method\":\"test-success\",
\"params\":{}
}"
                                     (make-lsp-state)
                                     (fake-sender-fun)))
      (is (expected-result-p #(reply #"{\"id\":99,\"result\":true}")))))

(deftest process-simple-initialize-message
  (with-fixture
   (is-equal (make-lsp-state initialized 'true)
             (lsp-proc:process-input (make-simple-initialize-request)
                                     (make-lsp-state)
                                     (fake-sender-fun)))
   (is (expected-result-p
        #(reply
          #"{\"id\":99,\"result\":{\"capabilities\":{\"completionProvider\":{\"resolveProvider\":true,\"triggerCharacters\":[\"(\",\":\",\"'\"]},\"textDocumentSync\":{\"openClose\":true,\"change\":1}},\"serverInfo\":{\"name\":\"lfe-ls\"}}}")))))

(deftest process-initialized-message
  (with-fixture
   (is-equal (make-lsp-state)
             (lsp-proc:process-input (make-initialized-notify-request)
                                     (make-lsp-state)
                                     (fake-sender-fun)))
   (is (expected-result-p #(noreply #"null")))))

(deftest process-textDocument/didOpen-message
  (with-fixture
   (is-equal #(lsp-state false #M(#"file:///foobar.lfe"
                                  #(document #"file:///foobar.lfe" 1 #"the-document-text")))
             (lsp-proc:process-input (make-simple-textDocument/didOpen-request)
                                     (make-lsp-state)
                                     (fake-sender-fun)))
   (is (expected-result-p #(noreply #"null")))))

(deftest process-textDocument/didOpen-message--second-doc
  (with-fixture
   (is-equal #(lsp-state false #M(#"file:///foobar.lfe"
                                  #(document #"file:///foobar.lfe" 1 #"the-document-text")
                                  #"file:///foobar2.lfe"
                                  #(document #"file:///foobar2.lfe" 2 #"the-document-text2")))
             (lsp-proc:process-input (make-simple-textDocument/didOpen-request)
                                     (make-lsp-state documents
                                                     #M(#"file:///foobar2.lfe"
                                                        #(document #"file:///foobar2.lfe"
                                                                   2
                                                                   #"the-document-text2")))
                                     (fake-sender-fun)))
   (is (expected-result-p #(noreply #"null")))))

(defun injected-document-state ()
  (make-lsp-state documents
                  #M(#"file:///foobar.lfe"
                     #(document #"file:///foobar.lfe" 1 #"the-document-text"))))

(deftest process-textDocument/didChange-message
  (with-fixture
   (is-equal #(lsp-state false #M(#"file:///foobar.lfe"
                                  #(document #"file:///foobar.lfe" 2 #"thedocument-text")))
             (lsp-proc:process-input (make-simple-textDocument/didChange-request)
                                     (injected-document-state)
                                     (fake-sender-fun)))
   (is (expected-result-p #(noreply #"null")))))

(deftest process-textDocument/didClose-message
  (with-fixture
   (is-equal #(lsp-state false #M())
             (lsp-proc:process-input (make-simple-textDocument/didClose-request)
                                     (injected-document-state)
                                     (fake-sender-fun)))
   (is (expected-result-p #(noreply #"null")))))

(deftest process-textDocument/completion-message--invoked-trigger
  (with-fixture
   (let ((new-state (lsp-proc:process-input
                     (make-compl-example-textDocument/didOpen-request)
                     (make-lsp-state)
                     (fake-sender-fun))))
     (meck:new 'completion-util)
     (meck:expect 'completion-util 'find-completions-at
                  (lambda (text position trigger-char)
                    (case trigger-char
                      ('null `(,(make-completion-item
                                 label #"defun"
                                 kind 2
                                 insert-text #"defun")))
                      (_ (error "Not expected trigger-char!")))))
     (is-equal new-state
               (lsp-proc:process-input
                (make-simple-textDocument/completion-request--invoked-trigger)
                new-state
                (fake-sender-fun)))
     (meck:unload 'completion-util)
     (is (expected-result-p
          #(reply
            #"{\"id\":99,\"result\":[{\"label\":\"defun\",\"kind\":2,\"detail\":\"\",\"insertText\":\"defun\"}]}"))))))

(deftest process-textDocument/completion-message--trigger-char
  (with-fixture
   (let ((new-state (lsp-proc:process-input
                     (make-compl-example-textDocument/didOpen-request)
                     (make-lsp-state)
                     (fake-sender-fun))))
     (meck:new 'completion-util)
     (meck:expect 'completion-util 'find-completions-at
                  (lambda (text position trigger-char)
                    (case trigger-char
                      ('null (error "Not expected trigger-char!"))
                      (#":" `(,(make-completion-item
                                label #"defun"
                                kind 2
                                detail #"foo"))))))
     (is-equal new-state
               (lsp-proc:process-input
                (make-simple-textDocument/completion-request--trigger-char)
                new-state
                (fake-sender-fun)))
     (meck:unload 'completion-util)
     (is (expected-result-p
          #(reply
            #"{\"id\":99,\"result\":[{\"label\":\"defun\",\"kind\":2,\"detail\":\"foo\"}]}"))))))

(deftest process-shutdown
  (with-fixture
   (is-equal (make-lsp-state)
             (lsp-proc:process-input (make-simple-shutdown-request)
                                     (make-lsp-state)
                                     (fake-sender-fun)))
   (is (expected-result-p #(reply #"{\"id\":null,\"result\":null}")))))

(deftest process-textDocument/didSave-message
  (with-fixture
   (let ((state (injected-document-state)))
     (meck:new 'compile-util)
     (meck:expect 'compile-util 'compile-file (lambda (file) #(ok ())))
    
     (is-equal state
               (lsp-proc:process-input (make-simple-textDocument/didSave-request)
                                       state
                                       (fake-sender-fun)))
     (is (expected-result-p #(notify #"{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/publishDiagnostics\",\"params\":{\"uri\":\"file:///foobar.lfe\",\"version\":1,\"diagnostics\":[]}}")))
     (is (meck:validate 'compile-util))
     (meck:unload 'compile-util))))

(defun make-simple-initialize-request ()
  #"{
\"jsonrpc\":\"2.0\",
\"id\":99,
\"method\":\"initialize\",
\"params\":{\"processId\":null,
\"clientInfo\":{\"name\":\"eglot\"},
\"rootPath\":null,\"rootUri\":null,
\"initializationOptions\":{},
\"capabilities\":{}}
}")

(defun make-initialized-notify-request ()
  #"{
\"jsonrpc\":\"2.0\",
\"method\":\"initialized\",
\"params\":{}
}")

(defun make-simple-textDocument/didOpen-request ()
  #"{
\"jsonrpc\":\"2.0\",
\"id\":99,
\"method\":\"textDocument/didOpen\",
\"params\":{\"textDocument\":{\"uri\":\"file:///foobar.lfe\",\"version\":1,\"languageId\":\"lfe\",\"text\":\"the-document-text\"}}
}")

(defun make-simple-textDocument/didChange-request ()
  #"{
\"jsonrpc\":\"2.0\",
\"id\":99,
\"method\":\"textDocument/didChange\",
\"params\":{\"textDocument\":{\"uri\":\"file:///foobar.lfe\",\"version\":2},\"contentChanges\":[{\"text\":\"thedocument-text\"}]}
}")

(defun make-simple-textDocument/didClose-request ()
  #"{
\"jsonrpc\":\"2.0\",
\"id\":99,
\"method\":\"textDocument/didClose\",
\"params\":{\"textDocument\":{\"uri\":\"file:///foobar.lfe\"}}
}")

(defun make-compl-example-textDocument/didOpen-request ()
  #"{
\"jsonrpc\":\"2.0\",
\"id\":99,
\"method\":\"textDocument/didOpen\",
\"params\":{\"textDocument\":{\"uri\":\"file:///foobar.lfe\",\"version\":1,\"languageId\":\"lfe\",\"text\":\"(de\"}}
}")

(defun make-simple-textDocument/didSave-request ()
  #"{
\"jsonrpc\":\"2.0\",
\"id\":99,
\"method\":\"textDocument/didSave\",
\"params\":{\"textDocument\":{\"uri\":\"file:///foobar.lfe\"}}
}")

(defun make-simple-textDocument/completion-request--invoked-trigger ()
  #"{
\"jsonrpc\":\"2.0\",
\"id\":99,
\"method\":\"textDocument/completion\",
\"params\":{\"textDocument\":{\"uri\":\"file:///foobar.lfe\"},\"position\":{\"line\":0,\"character\":3},\"context\":{\"triggerKind\":1}}
}")

(defun make-simple-textDocument/completion-request--trigger-char ()
  #"{
\"jsonrpc\":\"2.0\",
\"id\":99,
\"method\":\"textDocument/completion\",
\"params\":{\"textDocument\":{\"uri\":\"file:///foobar.lfe\"},\"position\":{\"line\":0,\"character\":3},\"context\":{\"triggerKind\":2,\"triggerCharacter\":\":\"}}
}")

(defun make-simple-shutdown-request ()
  #"{
\"jsonrpc\":\"2.0\",
\"method\":\"shutdown\",
\"params\":{}
}")

#|
initialize request:
{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"processId":null,"rootPath":"/Users/mbergmann/Development/MySources/lfe-ls/","rootUri":"file:///Users/mbergmann/Development/MySources/lfe-ls","initializationOptions":{},
"capabilities":{
"workspace":{"applyEdit":true,"executeCommand":{"dynamicRegistration":false},"workspaceEdit":{"documentChanges":false},"didChangeWatchedFiles":{"dynamicRegistration":true},"symbol":{"dynamicRegistration":false},"configuration":true},
"textDocument":{"synchronization":{"dynamicRegistration":false,"willSave":true,"willSaveWaitUntil":true,"didSave":true},"completion":{"dynamicRegistration":false,"completionItem":{"snippetSupport":true,"deprecatedSupport":true,"tagSupport":{"valueSet":[1]}},"contextSupport":true},"hover":{"dynamicRegistration":false,"contentFormat":["markdown","plaintext"]},"signatureHelp":{"dynamicRegistration":false,"signatureInformation":{"parameterInformation":{"labelOffsetSupport":true},"activeParameterSupport":true}},"references":{"dynamicRegistration":false},"definition":{"dynamicRegistration":false,"linkSupport":true},"declaration":{"dynamicRegistration":false,"linkSupport":true},"implementation":{"dynamicRegistration":false,"linkSupport":true},"typeDefinition":{"dynamicRegistration":false,"linkSupport":true},"documentSymbol":{"dynamicRegistration":false,"hierarchicalDocumentSymbolSupport":true,"symbolKind":{"valueSet":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26]}},"documentHighlight":{"dynamicRegistration":false},"codeAction":{"dynamicRegistration":false,"codeActionLiteralSupport":{"codeActionKind":{"valueSet":["quickfix","refactor","refactor.extract","refactor.inline","refactor.rewrite","source","source.organizeImports"]}},"isPreferredSupport":true},"formatting":{"dynamicRegistration":false},"rangeFormatting":{"dynamicRegistration":false},"rename":{"dynamicRegistration":false},"publishDiagnostics":{"relatedInformation":false,"codeDescriptionSupport":false,"tagSupport":{"valueSet":[1,2]}}},"experimental":{}}}}

textDocument/didOpen notify:
{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/didOpen\",\"params\":{\"textDocument\":{\"uri\":\"file:///Users/mbergmann/Development/MySources/lfe-ls/apps/lfe-ls/src/response-sender.lfe\",\"version\":0,\"languageId\":null,\"text\":\"(defmodule response-sender\"}}}

textDocument/didChange notify:
{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/didChange\",\"params\":{\"textDocument\":{\"uri\":\"file:///Users/mbergmann/Development/MySources/lfe-ls/apps/lfe-ls/test/lsp-proc-tests.lfe\",\"version\":1},\"contentChanges\":[{\"range\":{\"start\":{\"line\":141,\"character\":2},\"end\":{\"line\":141,\"character\":2}},\"rangeLength\":0,\"text\":\"\n\"}]}}

textDocument/completion request:
{\"jsonrpc\":\"2.0\",\"id\":12,\"method\":\"textDocument/completion\",\"params\\\":{\"textDocument\":{\"uri\":\"file:///Users/mbergmann/Development/MySources/lfe-ls/apps/lfe-ls/test/lfe-ls-tests.lfe\"},\"position\":{\"line\":86,\"character\":5},\"context\":{\"triggerKind\":1}}}

textDocument/didSave notify:
"{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/didSave\",\"params\":{\"text\":\"(defmodule...",\"textDocument\":{\"uri\":\"file:///Users/mbergmann/Development/MySources/lfe-ls/apps/lfe-ls/test/lfe-ls-tcp-integ-tests.lfe\"}}}"

shutdown request:
{\"jsonrpc\":\"2.0\",\"id\":40,\"method\":\"shutdown\",\"params\":null}
|#
