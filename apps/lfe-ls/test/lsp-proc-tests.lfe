(defmodule lsp-proc-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest error-on-decoding
  (is-equal `#(error "Unable to decode json!")
            (lsp-proc:process-input #"{\"Foo\"}")))

(deftest process-simple-message
  (is-equal `#(ok #"{\"id\":99,\"result\":true}")
            (lsp-proc:process-input #"{
\"jsonrpc\":\"2.0\",
\"id\":99,
\"method\":\"test-success\",
\"params\":{}
}")))
