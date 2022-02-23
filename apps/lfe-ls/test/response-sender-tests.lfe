(defmodule response-sender-tests
  (behaviour ltest-unit)
  (export (send 2)))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest reply
  (response-sender:send-response (MODULE) 'reply-device #(reply #"response")))

(deftest noreply
  (response-sender:send-response (MODULE) 'noreply-device #(noreply #"response")))

(deftest notify
  (response-sender:send-response (MODULE) 'notify-device #(notify #"notification")))

(defun send (device content)
  (case device
    ('reply-device
     (is-equal content #"Content-Length: 8\r\n\r\nresponse"))
    ('notify-device
     (is-equal content #"Content-Length: 12\r\n\r\nnotification"))))
