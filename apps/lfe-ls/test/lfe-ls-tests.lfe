(defmodule lfe-ls-tests
  (behaviour ltest-unit)
  (import (from lfe-ls
                (concat-binary 2))))

(include-lib "ltest/include/ltest-macros.lfe")

(include-lib "apps/lfe-ls/include/ls-model.lfe")


;; your test code here

(deftest create-lfe-ls
  (let ((`#(ok ,pid) (gen_server:start 'lfe-ls '#(other) '())))
    (io:format "~p~n" `(,pid))
    (gen_server:stop pid)))

(deftest test-receive-package--full-req
  (let* ((`#(ok ,pid) (gen_server:start 'lfe-ls '#(other) '()))
         (response (gen_server:call pid `#(received #"Content-Length: 5\r\n\r\nHello"))))
    (is-match `#(ok #(state nil #(req 5 5 #"Hello"))) response)
    (gen_server:stop pid)))

(deftest test-receive-package--partial-req
  (let* ((`#(ok ,pid) (gen_server:start 'lfe-ls '#(other) '()))
         (response1 (gen_server:call pid `#(received #"Content-Length: 10\r\n\r\nHello")))
         (response2 (gen_server:call pid `#(received #"World"))))
    (is-match `#(ok #(state nil #(req 10 5 #"Hello"))) response1)
    (is-match `#(ok #(state nil #(req 10 10 #"HelloWorld"))) response2)
    (gen_server:stop pid)))

(deftest test-receive-package--real-partial-rew
  (let* ((`#(ok ,pid) (gen_server:start 'lfe-ls '#(other) '()))
         (`#(ok ,state1) (gen_server:call pid `#(received ,(start-json-msg))))
         (`#(ok ,state2) (gen_server:call pid `#(received ,(json-msg-part2)))))
    (is-match (tuple 'state 'nil (tuple 'req 2027 1436 _)) state1)
    (is-match (tuple 'state 'nil (tuple 'req 2027 2027 _)) state2)
    (is-equal (json-msg-part1) (req-data (state-req state1)))
    (is-equal (full-json-msg) (req-data (state-req state2)))
    (gen_server:stop pid)))


(defun start-json-msg ()
  (concat-binary (preamble-json-msg) (json-msg-part1)))

(defun full-json-msg ()
  (concat-binary (json-msg-part1) (json-msg-part2)))

(defun preamble-json-msg ()
  #"Content-Length: 2027\r\n\r\n")

(defun json-msg-part1 ()
  #"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"processId\":null,\"rootPath\":\"/Users/mbergmann/Development/MySources/lfe-ls/\",\"rootUri\":\"file:///Users/mbergmann/Development/MySources/lfe-ls\",\"initializationOptions\":{},\"capabilities\":{\"workspace\":{\"applyEdit\":true,\"executeCommand\":{\"dynamicRegistration\":false},\"workspaceEdit\":{\"documentChanges\":false},\"didChangeWatchedFiles\":{\"dynamicRegistration\":true},\"symbol\":{\"dynamicRegistration\":false},\"configuration\":true},\"textDocument\":{\"synchronization\":{\"dynamicRegistration\":false,\"willSave\":true,\"willSaveWaitUntil\":true,\"didSave\":true},\"completion\":{\"dynamicRegistration\":false,\"completionItem\":{\"snippetSupport\":true,\"deprecatedSupport\":true,\"tagSupport\":{\"valueSet\":[1]}},\"contextSupport\":true},\"hover\":{\"dynamicRegistration\":false,\"contentFormat\":[\"markdown\",\"plaintext\"]},\"signatureHelp\":{\"dynamicRegistration\":false,\"signatureInformation\":{\"parameterInformation\":{\"labelOffsetSupport\":true},\"activeParameterSupport\":true}},\"references\":{\"dynamicRegistration\":false},\"definition\":{\"dynamicRegistration\":false,\"linkSupport\":true},\"declaration\":{\"dynamicRegistration\":false,\"linkSupport\":true},\"implementation\":{\"dynamicRegistration\":false,\"linkSupport\":true},\"typeDefinition\":{\"dynamicRegistration\":false,\"linkSupport\":true},\"documentSymbol\":{\"dynamicRegistration\":false,\"hierarchicalDocumentSymbolSupport\":true,\"symbolKind\":{\"valueSet\":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,1")

(defun json-msg-part2 ()
  #"6,17,18,19,20,21,22,23,24,25,26]}},\"documentHighlight\":{\"dynamicRegistration\":false},\"codeAction\":{\"dynamicRegistration\":false,\"codeActionLiteralSupport\":{\"codeActionKind\":{\"valueSet\":[\"quickfix\",\"refactor\",\"refactor.extract\",\"refactor.inline\",\"refactor.rewrite\",\"source\",\"source.organizeImports\"]}},\"isPreferredSupport\":true},\"formatting\":{\"dynamicRegistration\":false},\"rangeFormatting\":{\"dynamicRegistration\":false},\"rename\":{\"dynamicRegistration\":false},\"publishDiagnostics\":{\"relatedInformation\":false,\"codeDescriptionSupport\":false,\"tagSupport\":{\"valueSet\":[1,2]}}},\"experimental\":{}}}}")
