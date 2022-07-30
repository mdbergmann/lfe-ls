(defmodule lsp-ls-integ-tests
  (behaviour ltest-unit)
  (export (make-simple-textDocument/didSave-request 1)))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "apps/lfe-ls/include/utils.lfe")

(defmacro with-fixture body
  "This requires a repl with loaded project."
  `(progn
     ;;(application:set_env 'lfe-ls 'transport "tcp")
     ;;(application:set_env 'lfe-ls 'port 10567)
     ;; (lfe-ls-app:start 'foo 'bar)
     ;; (timer:sleep 1000)
     ,@body
     ;; (timer:sleep 1000)
     ;; (lfe-ls-app:stop)
     ;; (timer:sleep 500)
     ))

(deftest process-initialize-message
  (with-fixture 
   (let ((`#(ok ,socket) (gen_tcp:connect "127.0.0.1" 10567 '(#(active false)))))
     (gen_tcp:send socket (make-simple-initialize-request))
     (let (((tuple 'ok response) (gen_tcp:recv socket 0)))
       (is-equal "Content-Length: 198\r\n\r\n{\"id\":99,\"result\":{\"capabilities\":{\"completionProvider\":{\"resolveProvider\":false,\"triggerCharacters\":[\"(\",\":\",\"'\"]},\"textDocumentSync\":{\"openClose\":true,\"change\":1}},\"serverInfo\":{\"name\":\"lfe-ls\"}}}" response))
     (gen_tcp:close socket))))

(deftest process-completion-message
  (with-fixture
   (let ((`#(ok ,socket) (gen_tcp:connect "127.0.0.1" 10567 '(#(active false)))))
     (gen_tcp:send socket (make-simple-textDocument/completion-request))
     (let (((tuple 'ok response) (gen_tcp:recv socket 0)))
       (is (> (string:length response) 0)))
     (gen_tcp:close socket))))

(deftest process-didSave-message
  (with-fixture
   (let* ((`#(ok ,socket) (gen_tcp:connect "127.0.0.1" 10567 '(#(active false))))
          (`#(ok ,cwd) (file:get_cwd))
          (file (++ cwd "/compile-tmpls/error-no-include.lfe")))
     (gen_tcp:send socket (make-simple-textDocument/didSave-request file))
     (let (((tuple 'ok response) (gen_tcp:recv socket 0)))
       ;;(logger:notice "response: ~p" `(,response))
       (is-not-equal 'nomatch (string:find response "\"method\":\"textDocument/publishDiagnostics\""))
       (is-not-equal 'nomatch (string:find response "\"diagnostics\":[{\"range\":{\"start\":{\"line\":0,\"character\":0},\"end\":{\"line\":0,\"character\":0}},\"severity\":1,\"source\":\"lfe_lint\",\"message\":\"#(undefined_function #(my-fun 1))\"}]"))
       (gen_tcp:close socket)))))

(deftest process-completion-message-2
  (with-fixture
   (let ((`#(ok ,socket) (gen_tcp:connect "127.0.0.1" 10567 '(#(active false)))))
     (logger:notice "initializing...")
     (gen_tcp:send socket (make-simple-initialize-request))
     (gen_tcp:recv socket 0)
     (logger:notice "sending didOpen...")
     (gen_tcp:send socket (make-simple-textDocument/didOpen-request))
     (logger:notice "sending completion...")
     (gen_tcp:send socket (make-simple-textDocument/completion-request))
     (let (((tuple 'ok response) (gen_tcp:recv socket 0)))
       (is (> (string:length response) 0)))
     (gen_tcp:close socket))))

(defun make-simple-initialize-request ()
  #"Content-Length: 181\r\n\r\n{\"jsonrpc\":\"2.0\",\"id\":99,\"method\":\"initialize\",\"params\":{\"processId\":null,\"clientInfo\":{\"name\":\"eglot\"},\"rootPath\":null,\"rootUri\":null,\"initializationOptions\":{},\"capabilities\":{}}}")

(defun make-simple-textDocument/didOpen-request ()
  #"Content-Length: 175\r\n\r\n{
\"jsonrpc\":\"2.0\",
\"id\":99,
\"method\":\"textDocument/didOpen\",
\"params\":{\"textDocument\":{\"uri\":\"file:///foobar.lfe\",\"version\":1,\"languageId\":\"lfe\",\"text\":\"the-document-text\"}}
}")

(defun make-simple-textDocument/completion-request ()
  #"Content-Length: 184\r\n\r\n{
\"jsonrpc\":\"2.0\",
\"id\":99,
\"method\":\"textDocument/completion\",
\"params\":{\"textDocument\":{\"uri\":\"file:///foobar.lfe\"},\"position\":{\"line\":0,\"character\":3},\"context\":{\"triggerKind\":1}}
}")

(defun make-simple-textDocument/didSave-request (file)
  (let* ((content (lfe_io:format1 "{
\"jsonrpc\":\"2.0\",
\"id\":99,
\"method\":\"textDocument/didSave\",
\"params\":{\"textDocument\":{\"uri\":\"file://~s\"}}
}" `(,file)))
         (content-len (string:length content))
         (full-content (list_to_binary
                        (lfe_io:format1 "Content-Length: ~p\r\n\r\n~s"
                                        `(,content-len ,content)))))
    full-content))

#|
{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"processId\":null,\"rootPath\":\"/Users/mbergmann/Development/MySources/lfe-ls/\",\"rootUri\":\"file:///Users/mbergmann/Development/MySources/lfe-ls\",\"initializationOptions\":{},
\"capabilities\":{
\"workspace\":{\"applyEdit\":true,\"executeCommand\":{\"dynamicRegistration\":false},\"workspaceEdit\":{\"documentChanges\":false},\"didChangeWatchedFiles\":{\"dynamicRegistration\":true},\"symbol\":{\"dynamicRegistration\":false},\"configuration\":true},
\"textDocument\":{\"synchronization\":{\"dynamicRegistration\":false,\"willSave\":true,\"willSaveWaitUntil\":true,\"didSave\":true},\"completion\":{\"dynamicRegistration\":false,\"completionItem\":{\"snippetSupport\":true,\"deprecatedSupport\":true,\"tagSupport\":{\"valueSet\":[1]}},\"contextSupport\":true},\"hover\":{\"dynamicRegistration\":false,\"contentFormat\":[\"markdown\",\"plaintext\"]},\"signatureHelp\":{\"dynamicRegistration\":false,\"signatureInformation\":{\"parameterInformation\":{\"labelOffsetSupport\":true},\"activeParameterSupport\":true}},\"references\":{\"dynamicRegistration\":false},\"definition\":{\"dynamicRegistration\":false,\"linkSupport\":true},\"declaration\":{\"dynamicRegistration\":false,\"linkSupport\":true},\"implementation\":{\"dynamicRegistration\":false,\"linkSupport\":true},\"typeDefinition\":{\"dynamicRegistration\":false,\"linkSupport\":true},\"documentSymbol\":{\"dynamicRegistration\":false,\"hierarchicalDocumentSymbolSupport\":true,\"symbolKind\":{\"valueSet\":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26]}},\"documentHighlight\":{\"dynamicRegistration\":false},\"codeAction\":{\"dynamicRegistration\":false,\"codeActionLiteralSupport\":{\"codeActionKind\":{\"valueSet\":[\"quickfix\",\"refactor\",\"refactor.extract\",\"refactor.inline\",\"refactor.rewrite\",\"source\",\"source.organizeImports\"]}},\"isPreferredSupport\":true},\"formatting\":{\"dynamicRegistration\":false},\"rangeFormatting\":{\"dynamicRegistration\":false},\"rename\":{\"dynamicRegistration\":false},\"publishDiagnostics\":{\"relatedInformation\":false,\"codeDescriptionSupport\":false,\"tagSupport\":{\"valueSet\":[1,2]}}},\"experimental\":{}}}}
|#
