
(include-lib "apps/lfe-ls/include/lsp-model.lfe")

(defrecord req
  (data #""))

(defrecord ls-state
  (device 'nil)
  (req (make-req))
  (lsp-state (make-lsp-state)))
