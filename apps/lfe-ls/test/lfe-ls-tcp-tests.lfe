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

(deftest test-receive-package--req
  (with-fixture
   (meck:expect 'lsp-proc 'process-input (lambda (json-in lsp-state send-resp-fun)
                                           (make-lsp-state initialized 'true)))
   (meck:expect 'response-sender 'send-response (lambda (send-fun device response) 'ok))

   (let* ((response (gen_server:call pid `#(received #"Content-Length: 8\r\n\r\n{\"Ping\"}"))))
     (is-equal `#(ok #(ls-state nil #(req #"{\"Ping\"}") #(lsp-state true))) response)
     (is (meck:called 'lsp-proc 'process-input '(#"{\"Ping\"}" lsp-model)))
     (is (meck:validate 'lsp-proc))
     (is (meck:called 'response-sender 'send-response '(_ _ #"{\"Pong\"}")))
     (is (meck:validate 'response-sender)))))

(deftest test-receive-package--req--noreply
  (with-fixture
   (meck:expect 'lsp-proc 'process-input (lambda (json-in lsp-state send-resp-fun)
                                           `(make-lsp-state initialized 'true)))
   (meck:expect 'response-sender 'send-response (lambda (send-fun device response) 'ok))

   (let* ((response (gen_server:call pid `#(received #"Content-Length: 8\r\n\r\n{\"Ping\"}"))))
     (is-equal `#(ok #(ls-state nil #(req #"{\"Ping\"}") #(lsp-state true))) response)
     (is (meck:called 'lsp-proc 'process-input '(#"{\"Ping\"}" lsp-model)))
     (is (meck:validate 'lsp-proc))
     (is (meck:called 'response-sender 'send-response '(_ _ #(noreply #"{\"Pong\"}"))))
     (is-equal 0 (meck:num_calls 'response-sender '_ '_))
     (is (meck:validate 'response-sender)))))

(deftest test-receive-package--double-req--noreply
  (with-fixture
   (meck:expect 'lsp-proc 'process-input (lambda (json-in lsp-state send-resp-fun)
                                           `(make-lsp-state initialized 'true)))
   (meck:expect 'response-sender 'send-response (lambda (send-fun device response) 'ok))

   (let* ((response (gen_server:call pid `#(received #"Content-Length: 8\r\n\r\n{\"Ping\"}Content-Length: 9\r\n\r\n{\"Hello\"}"))))
     ;;(is-equal `#(ok #(ls-state nil #(req #"{\"Ping\"}") #(lsp-state true))) response)
     (is (meck:called 'lsp-proc 'process-input '(#"{\"Ping\"}" lsp-model)))
     (is (meck:called 'lsp-proc 'process-input '(#"{\"Hello\"}" lsp-model)))
     (is (meck:validate 'lsp-proc))
     (is (meck:called 'response-sender 'send-response '(_ _ #(noreply #"{\"Pong\"}"))))
     (is (meck:called 'response-sender 'send-response '(_ _ #(noreply #"{\"Pong\"}"))))
     (is-equal 0 (meck:num_calls 'response-sender '_ '_))
     (is (meck:validate 'response-sender)))))
