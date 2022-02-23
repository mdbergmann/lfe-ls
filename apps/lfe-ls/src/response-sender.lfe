(defmodule response-sender
  (export (send-response 3)))

(defun send-response (module device response)
  "Sends a response, or notification using an output device.
`module`: is the module that implements a `send/2` function.
`device`: is the device used for sending. This is just a pass through.
`response`: is a tuple of the 'type of' response (atom: reply, noreply, notify) and the json response itself."
  (logger:debug "Sending response...")
  (case response
    (`#(reply ,lsp-proc-output)
     (call module 'send device (%build-full-response lsp-proc-output)))
    (`#(noreply ,_)
     'ok)
    (`#(notify ,lsp-message)
     (call module 'send device (%build-full-response lsp-message)))))

(defun %build-full-response (json-response)
  (let* ((resp-size (byte_size json-response))
         (full-response (binary
                         (#"Content-Length: " binary)
                         ((erlang:integer_to_binary resp-size) binary)
                         (#"\r\n\r\n" binary)
                         (json-response binary))))
    full-response))
