(defmodule lsp-proc-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest error-on-decoding
  (is-equal `#(ok #"{\"id\":null,\"error\":{\"code\":-32700,\"message\":\"Error on parsing json!\"}}")
            (lsp-proc:process-input #"{\"Foo\"}")))

(deftest process-simple-message
  (is-equal `#(ok #"{\"id\":99,\"result\":true}")
            (lsp-proc:process-input #"{
\"jsonrpc\":\"2.0\",
\"id\":99,
\"method\":\"test-success\",
\"params\":{}
}")))

(deftest process-simple-initialize-message
  (is-equal `#(ok #"{\"id\":99,\"result\":{\"capabilities\":{},\"serverInfo\":{\"name\":\"lfe-ls\"}}}")
            (lsp-proc:process-input #"{
\"jsonrpc\":\"2.0\",
\"id\":99,
\"method\":\"initialize\",
\"params\":{\"processId\":null,
\"clientInfo\":{\"name\":\"eglot\"},
\"rootPath\":null,\"rootUri\":null,
\"initializationOptions\":{},
\"capabilities\":{}}
}")))

#|
{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"processId":null,"rootPath":"/Users/mbergmann/Development/MySources/lfe-ls/","rootUri":"file:///Users/mbergmann/Development/MySources/lfe-ls","initializationOptions":{},"capabilities":{"workspace":{"applyEdit":true,"executeCommand":{"dynamicRegistration":false},"workspaceEdit":{"documentChanges":false},"didChangeWatchedFiles":{"dynamicRegistration":true},"symbol":{"dynamicRegistration":false},"configuration":true},"textDocument":{"synchronization":{"dynamicRegistration":false,"willSave":true,"willSaveWaitUntil":true,"didSave":true},"completion":{"dynamicRegistration":false,"completionItem":{"snippetSupport":true,"deprecatedSupport":true,"tagSupport":{"valueSet":[1]}},"contextSupport":true},"hover":{"dynamicRegistration":false,"contentFormat":["markdown","plaintext"]},"signatureHelp":{"dynamicRegistration":false,"signatureInformation":{"parameterInformation":{"labelOffsetSupport":true},"activeParameterSupport":true}},"references":{"dynamicRegistration":false},"definition":{"dynamicRegistration":false,"linkSupport":true},"declaration":{"dynamicRegistration":false,"linkSupport":true},"implementation":{"dynamicRegistration":false,"linkSupport":true},"typeDefinition":{"dynamicRegistration":false,"linkSupport":true},"documentSymbol":{"dynamicRegistration":false,"hierarchicalDocumentSymbolSupport":true,"symbolKind":{"valueSet":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26]}},"documentHighlight":{"dynamicRegistration":false},"codeAction":{"dynamicRegistration":false,"codeActionLiteralSupport":{"codeActionKind":{"valueSet":["quickfix","refactor","refactor.extract","refactor.inline","refactor.rewrite","source","source.organizeImports"]}},"isPreferredSupport":true},"formatting":{"dynamicRegistration":false},"rangeFormatting":{"dynamicRegistration":false},"rename":{"dynamicRegistration":false},"publishDiagnostics":{"relatedInformation":false,"codeDescriptionSupport":false,"tagSupport":{"valueSet":[1,2]}}},"experimental":{}}}}
|#

#|
todos:

- generate proper json response
- generate proper json response on encoding error
- add tests for unrecognized request

|#
