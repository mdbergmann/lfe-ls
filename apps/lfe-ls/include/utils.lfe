
(defun concat-binary (bin1 bin2)
  (binary (bin1 binary) (bin2 binary)))

(defun binary-to-string (bin)
  (lists:flatten (io_lib:format "~p" `(,bin))))
