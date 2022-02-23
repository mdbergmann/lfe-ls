(defmodule lfe-ls-stdio-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

(include-lib "apps/lfe-ls/include/ls-model.lfe")

(defmacro with-fixture body
  `(case (file:open "/tmp/lfe-ls" '(write binary))
     (`#(ok ,dev)
      (file:write dev (full-msg))
      (file:close dev)
      (let ((`#(ok ,dev) (file:open "/tmp/lfe-ls" '(read append binary #(encoding latin1)))))
        (let ((`#(ok ,stdio) (lfe-ls-stdio:start_link dev)))
          (try
              (progn
                ,@body)
            (catch
              ((tuple type value stacktrace)
               (logger:error "Exception type: ~p, value: ~p, stacktrace: ~p"
                             `(,type ,value ,stacktrace)))))
          (erlang:exit stdio 'normal)))
      (file:delete "/tmp/lfe-ls"))))

(deftest send-initialize-receive-response
  (with-fixture
   (is (erlang:is_pid stdio))
   (timer:sleep 100)
   (case (file:read_file "/tmp/lfe-ls")
     (`#(ok ,bin-data)
      (is-equal `#(2074 31) (binary:match bin-data #"{\"id\":1,\"result\":{\"capabilities")))
     (_
      (is 'false)))
   ))

(defun start-json-msg ()
  (binary ((preamble-json-msg) binary) ((json-msg-part1) binary)))

(defun full-msg ()
  (binary ((preamble-json-msg) binary)
          ((json-msg-part1) binary)
          ((json-msg-part2) binary)))

(defun full-json-msg ()
  (binary ((json-msg-part1) binary) ((json-msg-part2) binary)))

(defun preamble-json-msg ()
  #"Content-Length: 2027\r\n\r\n")

(defun json-msg-part1 ()
  #"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"processId\":null,\"rootPath\":\"/Users/mbergmann/Development/MySources/lfe-ls/\",\"rootUri\":\"file:///Users/mbergmann/Development/MySources/lfe-ls\",\"initializationOptions\":{},\"capabilities\":{\"workspace\":{\"applyEdit\":true,\"executeCommand\":{\"dynamicRegistration\":false},\"workspaceEdit\":{\"documentChanges\":false},\"didChangeWatchedFiles\":{\"dynamicRegistration\":true},\"symbol\":{\"dynamicRegistration\":false},\"configuration\":true},\"textDocument\":{\"synchronization\":{\"dynamicRegistration\":false,\"willSave\":true,\"willSaveWaitUntil\":true,\"didSave\":true},\"completion\":{\"dynamicRegistration\":false,\"completionItem\":{\"snippetSupport\":true,\"deprecatedSupport\":true,\"tagSupport\":{\"valueSet\":[1]}},\"contextSupport\":true},\"hover\":{\"dynamicRegistration\":false,\"contentFormat\":[\"markdown\",\"plaintext\"]},\"signatureHelp\":{\"dynamicRegistration\":false,\"signatureInformation\":{\"parameterInformation\":{\"labelOffsetSupport\":true},\"activeParameterSupport\":true}},\"references\":{\"dynamicRegistration\":false},\"definition\":{\"dynamicRegistration\":false,\"linkSupport\":true},\"declaration\":{\"dynamicRegistration\":false,\"linkSupport\":true},\"implementation\":{\"dynamicRegistration\":false,\"linkSupport\":true},\"typeDefinition\":{\"dynamicRegistration\":false,\"linkSupport\":true},\"documentSymbol\":{\"dynamicRegistration\":false,\"hierarchicalDocumentSymbolSupport\":true,\"symbolKind\":{\"valueSet\":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,1")

(defun json-msg-part2 ()
  #"6,17,18,19,20,21,22,23,24,25,26]}},\"documentHighlight\":{\"dynamicRegistration\":false},\"codeAction\":{\"dynamicRegistration\":false,\"codeActionLiteralSupport\":{\"codeActionKind\":{\"valueSet\":[\"quickfix\",\"refactor\",\"refactor.extract\",\"refactor.inline\",\"refactor.rewrite\",\"source\",\"source.organizeImports\"]}},\"isPreferredSupport\":true},\"formatting\":{\"dynamicRegistration\":false},\"rangeFormatting\":{\"dynamicRegistration\":false},\"rename\":{\"dynamicRegistration\":false},\"publishDiagnostics\":{\"relatedInformation\":false,\"codeDescriptionSupport\":false,\"tagSupport\":{\"valueSet\":[1,2]}}},\"experimental\":{}}}}")

