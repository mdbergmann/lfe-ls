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

(defun %request-complete-p (req)
  (== (req-expected-len req) (req-current-len req)))

(defun %handle-new-req (msg req)
  (logger:debug "handle-new-req...")
  (case (binary:split msg (double-nl))
    (`(,len ,rest)
     (let* ((len-and-nl (binary (len binary)
                                ((double-nl) binary)))
            (proper-rest (string:prefix msg
                                        len-and-nl)))
       (make-req expected-len (erlang:binary_to_integer len)
                 current-len (byte_size proper-rest)
                 data proper-rest)))))

(defun %handle-partial-req (msg req)
  (logger:debug "handle-partial-req...")
  (clj:-> req
          (set-req-current-len (+ (req-current-len req) (byte_size msg)))
          (set-req-data (concat-binary (req-data req) msg))))

(defun %on-tcp-receive (msg state)
  "Handles data recived via tcp.
Can be 'call'ed or 'cast'.
Returns: #(ok new-state)"
  (logger:debug "Received msg len: ~p" `(,(byte_size msg)))
  (let* ((req (ls-state-req state))
         (socket (ls-state-device state))
         (lsp-state (ls-state-lsp-state state))
         (new-req (case (string:prefix msg "Content-Length: ")
                    ('nomatch (%handle-partial-req msg req))
                    (rest-msg (%handle-new-req rest-msg req)))))
    `#(ok ,(if (%request-complete-p new-req)
             (let ((complete-req (req-data new-req)))
               (logger:debug "Complete request: ~p" `(,complete-req))
               (logger:notice "Complete request of size: ~p" `(,(byte_size complete-req)))
               (let ((new-state (lsp-proc:process-input
                                 complete-req
                                 lsp-state
                                 (lambda (lsp-proc-result)
                                   (logger:debug "lsp output: ~p" `(,lsp-proc-result))
                                   (response-sender:send-response #'lfe-ls-tcp:send/2 socket lsp-proc-result)
                                   (logger:debug "Response sent!")))))
                 (clj:-> state
                         (set-ls-state-req (make-req)) ; reset
                         (set-ls-state-lsp-state new-state))))
             (set-ls-state-req state new-req)))))

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
   (let ((`#(,code ,new-state) (%on-tcp-receive msg state)))
     (inet:setopts socket '(#(active once)))
     (logger:debug "active once...done")
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

;;; our server API

(defun send (socket msg)
  (gen_tcp:send socket msg))

(defun pid ()
  (erlang:whereis (SERVER)))
