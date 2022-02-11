
(defrecord lsp-state
  (initialized 'false (boolean))
  (documents #M()))

(defrecord document
  (uri #"" (binary))
  (version 0 (integer))
  (text #"" (binary)))
