(defun tcar (t)
  (cl:elt 0 t))

(defun tcdr (t)
  (cl:elt 1 t))

(defun find-tkey (key lst)
  (cl:find-if (lambda (tup) (== key (tcar tup))) lst))

(defun concat-binary (bin1 bin2)
  (binary (bin1 binary) (bin2 binary)))

(defun binary-to-string (bin)
  (lists:flatten (io_lib:format "~p" `(,bin))))
