(defmodule lsp-proc-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest produce-output
  (is-equal `#(ok #"{\"Bar\":\"foo\"}") (lsp-proc:process-input #"{\"Foo\": \"bar\"}")))

(deftest error-on-decoding
  (is-equal `#(error "Unable to decode json!") (lsp-proc:process-input #"{\"Foo\"}")))
