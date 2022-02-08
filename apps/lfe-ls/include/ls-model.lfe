
(include-lib "apps/lfe-ls/include/lsp-model.lfe")

(defrecord req
  (expected-len 0)
  (current-len 0)
  (data #""))

(defrecord ls-state
  (socket 'nil)
  (req (make-req))
  (lsp-state (make-lsp-state)))
