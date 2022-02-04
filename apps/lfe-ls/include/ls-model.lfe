;;; Records for use by lfe-ls

(defrecord req
  (expected-len 0)
  (current-len 0)
  (data #""))

(defrecord state
  (socket 'nil)
  (req (make-req)))
