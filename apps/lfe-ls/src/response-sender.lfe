(defmodule response-sender
  (export (send-response 3)))

(defun send-response (send-fun device response)
  "Sends a response, or notification using an output device.
`send-fun`: is the lambda of 2-arity that takes arguments: device and lsp-response as arguments.
`device`: is the device the sends the response.
`response`: is a tuple of the 'type of' response (atom: reply, noreply, notify) and the json response itself."
  (logger:debug "Sending response via: ~p" `(,send-fun))
  (case response
    (`#(reply ,lsp-message)
     (funcall send-fun device (%build-full-response lsp-message)))
    (`#(noreply ,_)
     'ok)
    (`#(notify ,lsp-message)
     (funcall send-fun device (%build-full-response lsp-message)))))

(defun %build-full-response (json-response)
  (let* ((resp-size (byte_size json-response))
         (full-response (binary
                         (#"Content-Length: " binary)
                         ((erlang:integer_to_binary resp-size) binary)
                         (#"\r\n\r\n" binary)
                         (json-response binary))))
    full-response))
