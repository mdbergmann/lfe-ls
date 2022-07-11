(defmodule response-sender-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest reply
  (response-sender:send-response
   (lambda (device response)
     (is-equal 'reply-device device)
     (is-equal #"Content-Length: 8\r\n\r\nresponse" response))
   'reply-device
   #(reply #"response")))

(deftest noreply
  (response-sender:send-response
   (lambda (device response) 'ok)
   'noreply-device
   #(noreply #"response")))

(deftest notify
  (response-sender:send-response
   (lambda (device response)
     (is-equal 'notify-device device)
     (is-equal #"Content-Length: 12\r\n\r\nnotification" response))
   'notify-device
   #(notify #"notification")))
