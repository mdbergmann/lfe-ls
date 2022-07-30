(defmodule lfe-ls-tcp
  (behaviour gen_server)
  ;; gen_server implementation
  (export
   (start_link 1)
   (stop 0))
  ;; callback implementation
  (export
   (init 1)
   (handle_call 3)
   (handle_cast 2)
   (handle_info 2)
   (terminate 2)
   (code_change 3))
  ;; server API
  (export
   (send 2)
   (pid 0)))

(include-lib "apps/lfe-ls/include/utils.lfe")
(include-lib "apps/lfe-ls/include/ls-model.lfe")

;;; ----------------
;;; config functions
;;; ----------------

(defun SERVER () (MODULE))
(defun genserver-opts () '())

;;; -------------------------
;;; gen_server implementation
;;; -------------------------

(defun start_link (listen-socket)
  (logger:info "start_link (lfe-ls-tcp)")
  (gen_server:start_link `#(local ,(SERVER))
                         (MODULE)
                         `#(socket ,listen-socket)
                         (genserver-opts)))

(defun stop ()
  (gen_server:call (SERVER) 'stop))

;;; -----------------------
;;; callback implementation
;;; -----------------------

(defun init
  ((`#(socket ,listen-socket))
   (logger:info "init (lfe-ls-tcp)")
   (gen_server:cast (self) 'accept)
   `#(ok ,(make-ls-state device listen-socket)))
  ((`#(other))
   ;; used from test
   `#(ok ,(make-ls-state))))

(defun %accept-handler (state)
  (logger:debug "waiting for connection...")
  (let ((`#(ok ,accept-socket)
         (gen_tcp:accept (ls-state-device state))))
    (inet:setopts accept-socket '(#(active once)))
    (logger:notice "connection accepted (lfe-ls)")
    (lfe-ls-tcp-sup:start-socket)
    `#(noreply ,(set-ls-state-device state accept-socket))))

(defun double-nl () #"\r\n\r\n")

(defun %handle-new-req (msg socket)
  (logger:debug "handle-new-req...")
  (logger:debug "msg: ~p" `(,msg))
  (let* (((list len rest) (binary:split msg (double-nl)))
         (len-int (erlang:binary_to_integer len))
         (rest-len (byte_size rest))
         (len-diff (- len-int rest-len)))
    (logger:debug "New req content-len: ~p" `(,len-int))
    (logger:debug "rest-len: ~p, len-diff: ~p" `(,rest-len ,len-diff))
    (if (> len-diff 0)
      (let ((complete-req (binary (rest binary) ((recv socket len-diff) binary))))
        (logger:debug "complete-req: ~p" `(,complete-req))
        complete-req)
      rest)))

(defun %on-tcp-receive (msg state)
  "Handles data recived via tcp.
Can be 'call'ed or 'cast'.
Returns: #(ok new-state)"
  (logger:debug "Received msg len: ~p" `(,(byte_size msg)))
  (let* ((socket (ls-state-device state))
         (lsp-state (ls-state-lsp-state state))
         (new-req (case (string:prefix msg "Content-Length: ")
                    (rest-msg (%handle-new-req rest-msg socket)))))
    ;;(logger:debug "Complete request: ~p" `(,complete-req))
    (logger:notice "Complete request of size: ~p" `(,(byte_size new-req)))
    (let ((new-state (lsp-proc:process-input
                               new-req
                               lsp-state
                               (lambda (lsp-proc-result)
                                 ;;(logger:debug "lsp output: ~p" `(,lsp-proc-result))
                                 (response-sender:send-response #'lfe-ls-tcp:send/2 socket lsp-proc-result)
                                 (logger:debug "Response sent!")))))
      `#(ok ,(clj:-> state
                  (set-ls-state-req (make-req)) ; reset
                  (set-ls-state-lsp-state new-state))))))

(defun handle_cast
  "'receive is used to simulate TCP receival from a test"
  (('accept state)
   (%accept-handler state))
  ((`#(received ,msg) state)
   (let ((`#(,code ,new-state) (%on-tcp-receive msg state)))
     `#(noreply #(,code ,new-state) ,new-state)))
  ((_ state)
   (logger:debug "handle-cast, wildcard (server)")
   `#(noreply ,state)))

(defun handle_call
  "'receive is used to simulate TCP receival from a test"
  ((`#(received ,msg) _from state)
   (let ((`#(,code ,new-state) (%on-tcp-receive msg state)))
     `#(reply #(,code ,new-state) ,new-state)))
  (('stop _from state)
   `#(stop normal state))
  ((message _from state)
   `#(reply #(error "Unknown command.") ,state)))

(defun handle_info
  "LSP requests come in via 'tcp"
  ((`#(tcp ,socket ,msg) state)
   (inet:setopts socket '(#(active false)))
   (let ((`#(,code ,new-state) (%on-tcp-receive msg state)))
     (inet:setopts socket '(#(active once)))
     `#(noreply ,new-state)))
  ((`#(tcp_error ,_socket ,_) state)
   (logger:debug "tcp-error")
   `#(stop normal ,state))
  ((`#(tcp_closed ,_socket) state)
   (logger:debug "tcp-closed")
   `#(stop normal ,state))
  ((e state)
   (logger:warning "unexpected: ~p" `(,e))
   `#(noreply ,state)))

(defun terminate (_reason _state)
  'ok)

(defun code_change (_old-version state _extra)
  `#(ok ,state))

;;; private functions

(defun recv (socket len)
  (case (gen_tcp:recv socket len)
    ((tuple 'ok data)
     data)
    ((tuple 'error reason)
     (gen_server:stop (pid)))))

(defun send (socket msg)
  (gen_tcp:send socket msg))

(defun pid ()
  (erlang:whereis (SERVER)))
