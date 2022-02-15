(defmodule lsp-proc-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

(include-lib "apps/lfe-ls/include/utils.lfe")
(include-lib "apps/lfe-ls/include/lsp-model.lfe")

(deftest error-on-decoding
  (is-equal `#(#(reply
                 #"{\"id\":null,\"error\":{\"code\":-32700,\"message\":\"Error on parsing json!\"}}")
               ,(make-lsp-state))
            (lsp-proc:process-input #"{\"Foo\"}" (make-lsp-state))))

(deftest error-invalid-request--method-not-implemented
  (is-equal `#(#(noreply
                 #"{\"id\":99,\"error\":{\"code\":-32600,\"message\":\"Method not supported: 'not-supported'!\"}}")
               ,(make-lsp-state))
            (lsp-proc:process-input #"{
\"jsonrpc\":\"2.0\",
\"id\":99,
\"method\":\"not-supported\",
\"params\":{}
}" (make-lsp-state))))

(deftest error-invalid-request--method-not-implemented--no-id
  (is-equal `#(#(noreply
                 #"{\"id\":null,\"error\":{\"code\":-32600,\"message\":\"Method not supported: 'not-supported'!\"}}")
               ,(make-lsp-state))
            (lsp-proc:process-input #"{
\"jsonrpc\":\"2.0\",
\"method\":\"not-supported\",
\"params\":{}
}" (make-lsp-state))))

(deftest error-invalid-request--invalid-request
  (is-equal `#(#(reply
                 #"{\"id\":null,\"error\":{\"code\":-32600,\"message\":\"Invalid LSP header!\"}}")
               ,(make-lsp-state))
            (lsp-proc:process-input #"{
\"jsonrpc\":\"2.0\",
\"method\":\"not-supported\"}" (make-lsp-state))))

(deftest process-simple-message
  (is-equal `#(#(reply #"{\"id\":99,\"result\":true}")
               ,(make-lsp-state))
            (lsp-proc:process-input #"{
\"jsonrpc\":\"2.0\",
\"id\":99,
\"method\":\"test-success\",
\"params\":{}
}" (make-lsp-state))))

(deftest process-simple-initialize-message
  (is-equal `#(#(reply
                 #"{\"id\":99,\"result\":{\"capabilities\":{\"completionProvider\":{\"resolveProvider\":true,\"triggerCharacters\":[\"(\",\":\",\"'\"]},\"textDocumentSync\":{\"openClose\":true,\"change\":1}},\"serverInfo\":{\"name\":\"lfe-ls\"}}}")
               ,(make-lsp-state initialized 'true))
            (lsp-proc:process-input (make-simple-initialize-request)
                                    (make-lsp-state))))

(deftest process-initialized-message
  (is-equal `#(#(noreply #"null") ,(make-lsp-state))
            (lsp-proc:process-input (make-initialized-notify-request)
                                    (make-lsp-state))))

(deftest process-textDocument/didOpen-message
  (is-equal `#(#(noreply #"null")
               #(lsp-state false #M(#"file:///foobar.lfe"
                                    #(document #"file:///foobar.lfe" 1 #"the-document-text"))))
            (lsp-proc:process-input (make-simple-textDocument/didOpen-request)
                                    (make-lsp-state))))

(deftest process-textDocument/didOpen-message--second-doc
  (is-equal `#(#(noreply #"null")
               #(lsp-state false #M(#"file:///foobar.lfe"
                                    #(document #"file:///foobar.lfe" 1 #"the-document-text")
                                    #"file:///foobar2.lfe"
                                    #(document #"file:///foobar2.lfe" 2 #"the-document-text2"))))
            (lsp-proc:process-input (make-simple-textDocument/didOpen-request)
                                    (make-lsp-state documents
                                                    #M(#"file:///foobar2.lfe"
                                                       #(document #"file:///foobar2.lfe"
                                                                  2
                                                                  #"the-document-text2"))))))

(defun injected-document-state ()
  (let* ((state (make-lsp-state))
         (new-state (tcdr (lsp-proc:process-input (make-simple-textDocument/didOpen-request)
                                                  state))))
    new-state))

(deftest process-textDocument/didChange-message
  (is-equal `#(#(noreply #"null")
               #(lsp-state false #M(#"file:///foobar.lfe"
                                    #(document #"file:///foobar.lfe" 2 #"thedocument-text"))))
            (lsp-proc:process-input (make-simple-textDocument/didChange-request)
                                    (injected-document-state))))

(deftest process-textDocument/didClose-message
  (is-equal `#(#(noreply #"null")
               #(lsp-state false #M()))
            (lsp-proc:process-input (make-simple-textDocument/didClose-request)
                                    (injected-document-state))))

(deftest process-textDocument/completion-message--invoked-trigger
  (let* ((state (make-lsp-state))
         (new-state (tcdr (lsp-proc:process-input (make-compl-example-textDocument/didOpen-request)
                                                  state))))
    (meck:new 'completion-util)
    (meck:expect 'completion-util 'find-completions-at (lambda (text position trigger-char)
                                                         (case trigger-char
                                                           ('null `(,(make-completion-item
                                                                      label #"defun"
                                                                      kind 2
                                                                      insert-text #"defun")))
                                                           (_ (error "Not expected trigger-char!")))))
    (is-equal `#(#(reply
                   #"{\"id\":99,\"result\":[{\"label\":\"defun\",\"kind\":2,\"detail\":\"\",\"insertText\":\"defun\"}]}")
                 ,new-state)
              (lsp-proc:process-input (make-simple-textDocument/completion-request--invoked-trigger)
                                      new-state))
    (meck:unload 'completion-util)))

(deftest process-textDocument/completion-message--trigger-char
  (let* ((state (make-lsp-state))
         (new-state (tcdr (lsp-proc:process-input (make-compl-example-textDocument/didOpen-request)
                                                  state))))
    (meck:new 'completion-util)
    (meck:expect 'completion-util 'find-completions-at (lambda (text position trigger-char)
                                                         (case trigger-char
                                                           ('null (error "Not expected trigger-char!"))
                                                           (#":" `(,(make-completion-item
                                                                     label #"defun"
                                                                     kind 2
                                                                     detail #"foo"))))))
    (is-equal `#(#(reply
                   #"{\"id\":99,\"result\":[{\"label\":\"defun\",\"kind\":2,\"detail\":\"foo\"}]}")
                 ,new-state)
              (lsp-proc:process-input (make-simple-textDocument/completion-request--trigger-char)
                                      new-state))
    (meck:unload 'completion-util)))

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

(defun make-compl-example-textDocument/didOpen-request ()
  #"{
\"jsonrpc\":\"2.0\",
\"id\":99,
\"method\":\"textDocument/didOpen\",
\"params\":{\"textDocument\":{\"uri\":\"file:///foobar.lfe\",\"version\":1,\"languageId\":\"lfe\",\"text\":\"(de\"}}
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

shutdown request:
{\"jsonrpc\":\"2.0\",\"id\":40,\"method\":\"shutdown\",\"params\":null}
|#
