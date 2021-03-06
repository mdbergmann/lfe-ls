(defmodule lfe-ls-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

(include-lib "apps/lfe-ls/include/utils.lfe")
(include-lib "apps/lfe-ls/include/ls-model.lfe")

(defmacro with-fixture body
  `(progn
     (meck:new 'lsp-proc)
     (meck:new 'response-sender)
     (let ((`#(ok ,pid) (gen_server:start 'lfe-ls-tcp '#(other) '()))
           (lsp-model (make-lsp-state)))
       (try
           (progn
             ,@body)
         (catch
           (_ (io:format "Exception: ~n"))))
       (gen_server:stop pid))
     (meck:unload 'lsp-proc)
     (meck:unload 'response-sender)))

(deftest create-lfe-ls
  (let ((`#(ok ,pid) (gen_server:start 'lfe-ls-tcp '#(other) '())))
    (io:format "~p~n" `(,pid))
    (gen_server:stop pid)))

(deftest test-receive-package--full-req
  (with-fixture
   (meck:expect 'lsp-proc 'process-input (lambda (json-in lsp-state send-resp-fun)
                                           (make-lsp-state initialized 'true)))
   (meck:expect 'response-sender 'send-response (lambda (_sender-pid response) 'ok))

   (let* ((response (gen_server:call pid `#(received #"Content-Length: 8\r\n\r\n{\"Ping\"}"))))
     (is-equal `#(ok #(ls-state nil #(req 8 8 #"{\"Ping\"}") #(lsp-state true))) response)
     (is (meck:called 'lsp-proc 'process-input '(#"{\"Ping\"}" lsp-model)))
     (is (meck:validate 'lsp-proc))
     (is (meck:called 'response-sender 'send-response '(_ #"{\"Pong\"}")))
     (is (meck:validate 'response-sender)))))

(deftest test-receive-package--full-req--noreply
  (with-fixture
   (meck:expect 'lsp-proc 'process-input (lambda (json-in lsp-state send-resp-fun)
                                           `(make-lsp-state initialized 'true)))
   (meck:expect 'response-sender 'send-response (lambda (_sender-pid response) 'ok))

   (let* ((response (gen_server:call pid `#(received #"Content-Length: 8\r\n\r\n{\"Ping\"}"))))
     (is-equal `#(ok #(ls-state nil #(req 8 8 #"{\"Ping\"}") #(lsp-state true))) response)
     (is (meck:called 'lsp-proc 'process-input '(#"{\"Ping\"}" lsp-model)))
     (is (meck:validate 'lsp-proc))
     (is (meck:called 'response-sender 'send-response '(_ #(noreply #"{\"Pong\"}"))))
     (is-equal 0 (meck:num_calls 'response-sender '_ '_))
     (is (meck:validate 'response-sender)))))

(deftest test-receive-package--incomplete-req--replaced-by-new-req
  (with-fixture
   (meck:expect 'lsp-proc 'process-input (lambda (json-in lsp-state send-resp-fun)
                                           lsp-state))
   (meck:expect 'response-sender 'send-response (lambda (_sender-pid response) 'ok))

   (let* ((_ (gen_server:call pid `#(received #"Content-Length: 13\r\n\r\n{\"Hello\"}")))
          (response (gen_server:call pid `#(received #"Content-Length: 8\r\n\r\n{\"Ping\"}"))))
     (is-equal `#(ok #(ls-state nil #(req 8 8 #"{\"Ping\"}") _)) response)
     (is (meck:called 'lsp-proc 'process-input '(#"{\"Ping\"}" lsp-model)))
     (is (meck:validate 'lsp-proc))
     (is (meck:called 'response-sender 'send-response '(_ #(reply #"{\"Pong\"}"))))
     (is (meck:validate 'response-sender)))))

(deftest test-receive-package--partial-req
  (with-fixture
   (meck:expect 'lsp-proc 'process-input (lambda (json-in lsp-state send-resp-fun)
                                           lsp-state))
   (meck:expect 'response-sender 'send-response (lambda (_sender-pid response) 'ok))

   (let* ((response1 (gen_server:call pid `#(received #"Content-Length: 14\r\n\r\n{\"Hello")))
          (response2 (gen_server:call pid `#(received #"World\"}"))))
     (is-match `#(ok #(ls-state nil #(req 14 7 #"{\"Hello") _)) response1)
     (is-match `#(ok #(ls-state nil #(req 14 14 #"{\"HelloWorld\"}") _)) response2)
     (is (meck:called 'lsp-proc 'process-input '(#"{\"HelloWorld\"}" lsp-model)))
     (is (meck:validate 'lsp-proc))
     (is (meck:called 'response-sender 'send-response '(_ #(reply #"{\"WorldHello\"}"))))
     (is (meck:validate 'response-sender)))))

(deftest test-receive-package--real-partial-req
  (with-fixture
   (meck:expect 'lsp-proc 'process-input (lambda (json-in lsp-state send-resp-fun)
                                           lsp-state))
   (meck:expect 'response-sender 'send-response (lambda (_sender-pid response) 'ok))

   (let* ((`#(ok ,state1) (gen_server:call pid `#(received ,(start-json-msg))))
          (`#(ok ,state2) (gen_server:call pid `#(received ,(json-msg-part2)))))
     (is-match (tuple 'ls-state 'nil (tuple 'req 2027 1436 _) _) state1)
     (is-match (tuple 'ls-state 'nil (tuple 'req 2027 2027 _) _) state2)
     (is-equal (json-msg-part1) (req-data (ls-state-req state1)))
     (is-equal (full-json-msg) (req-data (ls-state-req state2))))))

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

