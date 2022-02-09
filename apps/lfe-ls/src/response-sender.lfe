(defmodule response-sender
  (export (send-response 2)))

(defun send-response (socket json-response)
  (logger:debug "Sending response...")
  (let* ((resp-size (byte_size json-response))
         (full-response (binary
                         (#"Content-Length: " binary)
                         ((erlang:integer_to_binary resp-size) binary)
                         (#"\r\n\r\n" binary)
                         (json-response binary))))
    (gen_tcp:send socket full-response)))
