(defmodule lsp-proc-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

(include-lib "apps/lfe-ls/include/lsp-model.lfe")

(deftest error-on-decoding
  (is-equal `#(ok #"{\"id\":null,\"error\":{\"code\":-32700,\"message\":\"Error on parsing json!\"}}"
                  ,(make-lsp-state))
            (lsp-proc:process-input #"{\"Foo\"}" (make-lsp-state))))

(deftest error-invalid-request--method-not-implemented
  (is-equal `#(ok #"{\"id\":99,\"error\":{\"code\":-32600,\"message\":\"Method not supported: 'not-supported'!\"}}"
                  ,(make-lsp-state))
            (lsp-proc:process-input #"{
\"jsonrpc\":\"2.0\",
\"id\":99,
\"method\":\"not-supported\",
\"params\":{}
}" (make-lsp-state))))

(deftest error-invalid-request--method-not-implemented--no-id
  (is-equal `#(ok #"{\"id\":null,\"error\":{\"code\":-32600,\"message\":\"Method not supported: 'not-supported'!\"}}"
                  ,(make-lsp-state))
            (lsp-proc:process-input #"{
\"jsonrpc\":\"2.0\",
\"method\":\"not-supported\",
\"params\":{}
}" (make-lsp-state))))

(deftest error-invalid-request--invalid-request
  (is-equal `#(ok #"{\"id\":null,\"error\":{\"code\":-32600,\"message\":\"Invalid LSP header!\"}}"
                  ,(make-lsp-state))
            (lsp-proc:process-input #"{
\"jsonrpc\":\"2.0\",
\"method\":\"not-supported\"}" (make-lsp-state))))

(deftest process-simple-message
  (is-equal `#(ok #"{\"id\":99,\"result\":true}"
                  ,(make-lsp-state))
            (lsp-proc:process-input #"{
\"jsonrpc\":\"2.0\",
\"id\":99,
\"method\":\"test-success\",
\"params\":{}
}" (make-lsp-state))))

(deftest process-simple-initialize-message
  (is-equal `#(ok #"{\"id\":99,\"result\":{\"capabilities\":{\"textDocument\":{\"completion\":{\"dynamicRegistration\":false}}},\"serverInfo\":{\"name\":\"lfe-ls\"}}}"
                  ,(make-lsp-state initialized 'true))
            (lsp-proc:process-input (make-simple-initialize-request)
                                    (make-lsp-state))))

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


#|
{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"processId":null,"rootPath":"/Users/mbergmann/Development/MySources/lfe-ls/","rootUri":"file:///Users/mbergmann/Development/MySources/lfe-ls","initializationOptions":{},
"capabilities":{
"workspace":{"applyEdit":true,"executeCommand":{"dynamicRegistration":false},"workspaceEdit":{"documentChanges":false},"didChangeWatchedFiles":{"dynamicRegistration":true},"symbol":{"dynamicRegistration":false},"configuration":true},
"textDocument":{"synchronization":{"dynamicRegistration":false,"willSave":true,"willSaveWaitUntil":true,"didSave":true},"completion":{"dynamicRegistration":false,"completionItem":{"snippetSupport":true,"deprecatedSupport":true,"tagSupport":{"valueSet":[1]}},"contextSupport":true},"hover":{"dynamicRegistration":false,"contentFormat":["markdown","plaintext"]},"signatureHelp":{"dynamicRegistration":false,"signatureInformation":{"parameterInformation":{"labelOffsetSupport":true},"activeParameterSupport":true}},"references":{"dynamicRegistration":false},"definition":{"dynamicRegistration":false,"linkSupport":true},"declaration":{"dynamicRegistration":false,"linkSupport":true},"implementation":{"dynamicRegistration":false,"linkSupport":true},"typeDefinition":{"dynamicRegistration":false,"linkSupport":true},"documentSymbol":{"dynamicRegistration":false,"hierarchicalDocumentSymbolSupport":true,"symbolKind":{"valueSet":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26]}},"documentHighlight":{"dynamicRegistration":false},"codeAction":{"dynamicRegistration":false,"codeActionLiteralSupport":{"codeActionKind":{"valueSet":["quickfix","refactor","refactor.extract","refactor.inline","refactor.rewrite","source","source.organizeImports"]}},"isPreferredSupport":true},"formatting":{"dynamicRegistration":false},"rangeFormatting":{"dynamicRegistration":false},"rename":{"dynamicRegistration":false},"publishDiagnostics":{"relatedInformation":false,"codeDescriptionSupport":false,"tagSupport":{"valueSet":[1,2]}}},"experimental":{}}}}
|#

#|
2022-02-09 22:04:09 INFO <0.1080.0> [:] \x{25B8} text="Complete request of size: 52" 
2022-02-09 22:04:09 DEBUG <0.1080.0> [:] \x{25B8} text="json-input: [{<<\"jsonrpc\">>,<<\"2.0\">>},\
             {<<\"method\">>,<<\"initialized\">>},\
             {<<\"params\">>,[{}]}]" 
2022-02-09 22:04:09 INFO <0.1080.0> [:] \x{25B8} text="Complete request of size: 2027" 
2022-02-09 22:04:09 DEBUG <0.1080.0> [:] \x{25B8} text="json-input: [{<<\"jsonrpc\">>,<<\"2.0\">>},\
             {<<\"id\">>,1},\
             {<<\"method\">>,<<\"initialize\">>},\
             {<<\"params\">>,\
              [{<<\"processId\">>,null},\
               {<<\"rootPath\">>,\
                <<\"/Users/mbergmann/Development/MySources/lfe-ls/\">>},\
               {<<\"rootUri\">>,\
                <<\"file:///Users/mbergmann/Development/MySources/lfe-ls\">>},\
               {<<\"initializationOptions\">>,[{}]},\
               {<<\"capabilities\">>,\
                [{<<\"workspace\">>,\
                  [{<<\"applyEdit\">>,true},\
                   {<<\"executeCommand\">>,[{<<\"dynamicRegistration\">>,false}]},\
                   {<<\"workspaceEdit\">>,[{<<\"documentChanges\">>,false}]},\
                   {<<\"didChangeWatchedFiles\">>,\
                    [{<<\"dynamicRegistration\">>,true}]},\
                   {<<\"symbol\">>,[{<<\"dynamicRegistration\">>,false}]},\
                   {<<\"configuration\">>,true}]},\
                 {<<\"textDocument\">>,\
                  [{<<\"synchronization\">>,\
                    [{<<\"dynamicRegistration\">>,false},\
                     {<<\"willSave\">>,true},\
                     {<<\"willSaveWaitUntil\">>,true},\
                     {<<\"didSave\">>,true}]},\
                   {<<\"completion\">>,\
                    [{<<\"dynamicRegistration\">>,false},\
                     {<<\"completionItem\">>,\
                      [{<<\"snippetSupport\">>,true},\
                       {<<\"deprecatedSupport\">>,true},\
                       {<<\"tagSupport\">>,[{<<\"valueSet\">>,[1]}]}]},\
                     {<<\"contextSupport\">>,true}]},\
                   {<<\"hover\">>,\
                    [{<<\"dynamicRegistration\">>,false},\
                     {<<\"contentFormat\">>,[<<\"markdown\">>,<<\"plaintext\">>]}]},\
                   {<<\"signatureHelp\">>,\
                    [{<<\"dynamicRegistration\">>,false},\
                     {<<\"signatureInformation\">>,\
                      [{<<\"parameterInformation\">>,\
                        [{<<\"labelOffsetSupport\">>,true}]},\
                       {<<\"activeParameterSupport\">>,true}]}]},\
                   {<<\"references\">>,[{<<\"dynamicRegistration\">>,false}]},\
                   {<<\"definition\">>,\
                    [{<<\"dynamicRegistration\">>,false},\
                     {<<\"linkSupport\">>,true}]},\
                   {<<\"declaration\">>,\
                    [{<<\"dynamicRegistration\">>,false},\
                     {<<\"linkSupport\">>,true}]},\
                   {<<\"implementation\">>,\
                    [{<<\"dynamicRegistration\">>,false},\
                     {<<\"linkSupport\">>,true}]},\
                   {<<\"typeDefinition\">>,\
                    [{<<\"dynamicRegistration\">>,false},\
                     {<<\"linkSupport\">>,true}]},\
                   {<<\"documentSymbol\">>,\
                    [{<<\"dynamicRegistration\">>,false},\
                     {<<\"hierarchicalDocumentSymbolSupport\">>,true},\
                     {<<\"symbolKind\">>,\
                      [{<<\"valueSet\">>,\
                        [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,\
                         21,22,23,24,25,26]}]}]},\
                   {<<\"documentHighlight\">>,\
                    [{<<\"dynamicRegistration\">>,false}]},\
                   {<<\"codeAction\">>,\
                    [{<<\"dynamicRegistration\">>,false},\
                     {<<\"codeActionLiteralSupport\">>,\
                      [{<<\"codeActionKind\">>,\
                        [{<<\"valueSet\">>,\
                          [<<\"quickfix\">>,<<\"refactor\">>,\
                           <<\"refactor.extract\">>,<<\"refactor.inline\">>,\
                           <<\"refactor.rewrite\">>,<<\"source\">>,\
                           <<\"source.organizeImports\">>]}]}]},\
                     {<<\"isPreferredSupport\">>,true}]},\
                   {<<\"formatting\">>,[{<<\"dynamicRegistration\">>,false}]},\
                   {<<\"rangeFormatting\">>,[{<<\"dynamicRegistration\">>,false}]},\
                   {<<\"rename\">>,[{<<\"dynamicRegistration\">>,false}]},\
                   {<<\"publishDiagnostics\">>,\
                    [{<<\"relatedInformation\">>,false},\
                     {<<\"codeDescriptionSupport\">>,false},\
                     {<<\"tagSupport\">>,[{<<\"valueSet\">>,[1,2]}]}]}]},\
{<<\"experimental\">>,[{}]}]}]}]"
|#
