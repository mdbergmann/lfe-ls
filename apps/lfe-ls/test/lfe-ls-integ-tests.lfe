(defmodule lsp-ls-integ-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "apps/lfe-ls/include/utils.lfe")

(deftest process-initialize-message
  (let ((`#(ok ,socket) (gen_tcp:connect "127.0.0.1" 5555 '(#(active false)))))
    (gen_tcp:send socket (make-simple-initialize-request))
    (let (((tuple 'ok response) (gen_tcp:recv socket 0)))
      (is-equal "Content-Length: 197\r\n\r\n{\"id\":99,\"result\":{\"capabilities\":{\"completionProvider\":{\"resolveProvider\":true,\"triggerCharacters\":[\"(\",\":\",\"'\"]},\"textDocumentSync\":{\"openClose\":true,\"change\":1}},\"serverInfo\":{\"name\":\"lfe-ls\"}}}" response))
    (gen_tcp:close socket)))

(deftest process-completion-message
  (let ((`#(ok ,socket) (gen_tcp:connect "127.0.0.1" 5555 '(#(active false)))))
    (logger:notice "initializing...")
    (gen_tcp:send socket (make-simple-initialize-request))
    (gen_tcp:recv socket 0)
    (logger:notice "sending didOpen...")
    (gen_tcp:send socket (make-simple-textDocument/didOpen-request))
    (logger:notice "sending completion...")
    (gen_tcp:send socket (make-simple-textDocument/completion-request))
    (let (((tuple 'ok response) (gen_tcp:recv socket 0)))
      (is-equal "Content-Length: 47\r\n\r\n{\"id\":99,\"result\":[{\"label\":\"defun\",\"kind\":2}]}" response))
    (gen_tcp:close socket)))

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


#|
{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"processId\":null,\"rootPath\":\"/Users/mbergmann/Development/MySources/lfe-ls/\",\"rootUri\":\"file:///Users/mbergmann/Development/MySources/lfe-ls\",\"initializationOptions\":{},
\"capabilities\":{
\"workspace\":{\"applyEdit\":true,\"executeCommand\":{\"dynamicRegistration\":false},\"workspaceEdit\":{\"documentChanges\":false},\"didChangeWatchedFiles\":{\"dynamicRegistration\":true},\"symbol\":{\"dynamicRegistration\":false},\"configuration\":true},
\"textDocument\":{\"synchronization\":{\"dynamicRegistration\":false,\"willSave\":true,\"willSaveWaitUntil\":true,\"didSave\":true},\"completion\":{\"dynamicRegistration\":false,\"completionItem\":{\"snippetSupport\":true,\"deprecatedSupport\":true,\"tagSupport\":{\"valueSet\":[1]}},\"contextSupport\":true},\"hover\":{\"dynamicRegistration\":false,\"contentFormat\":[\"markdown\",\"plaintext\"]},\"signatureHelp\":{\"dynamicRegistration\":false,\"signatureInformation\":{\"parameterInformation\":{\"labelOffsetSupport\":true},\"activeParameterSupport\":true}},\"references\":{\"dynamicRegistration\":false},\"definition\":{\"dynamicRegistration\":false,\"linkSupport\":true},\"declaration\":{\"dynamicRegistration\":false,\"linkSupport\":true},\"implementation\":{\"dynamicRegistration\":false,\"linkSupport\":true},\"typeDefinition\":{\"dynamicRegistration\":false,\"linkSupport\":true},\"documentSymbol\":{\"dynamicRegistration\":false,\"hierarchicalDocumentSymbolSupport\":true,\"symbolKind\":{\"valueSet\":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26]}},\"documentHighlight\":{\"dynamicRegistration\":false},\"codeAction\":{\"dynamicRegistration\":false,\"codeActionLiteralSupport\":{\"codeActionKind\":{\"valueSet\":[\"quickfix\",\"refactor\",\"refactor.extract\",\"refactor.inline\",\"refactor.rewrite\",\"source\",\"source.organizeImports\"]}},\"isPreferredSupport\":true},\"formatting\":{\"dynamicRegistration\":false},\"rangeFormatting\":{\"dynamicRegistration\":false},\"rename\":{\"dynamicRegistration\":false},\"publishDiagnostics\":{\"relatedInformation\":false,\"codeDescriptionSupport\":false,\"tagSupport\":{\"valueSet\":[1,2]}}},\"experimental\":{}}}}
|#
