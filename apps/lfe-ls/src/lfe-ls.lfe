(defmodule lfe-ls
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
   (pid 0)
   (echo 1)
   (send 1)))

;;; ----------------
;;; config functions
;;; ----------------

(defun SERVER () (MODULE))
(defun genserver-opts () '())
(defun unknown-command () #(error "Unknown command."))

;;; -----------------
;;; records
;;; -----------------

(defrecord req
  (expected-len 0)
  (current-len 0)
  (data #""))

(defrecord state
  (socket 'nil)
  (req (make-req)))

;;; -------------------------
;;; gen_server implementation
;;; -------------------------

(defun start_link (listen-socket)
  (logger:debug "start_link (server)")
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
   (logger:debug "init (server)")
   (gen_server:cast (self) 'accept)
   `#(ok ,(make-state socket listen-socket)))
  ((`#(other))
   ;; used from test
   `#(ok ,(make-state))))

(defun handle_cast
  (('accept state)
   (%accept-handler state))
  ((_ state)
   (logger:debug "handle-cast, wildcard (server)")
   `#(noreply ,state)))

(defun %accept-handler (state)
  (logger:debug "waiting for connection...")
  (let ((`#(ok ,accept-socket)
         (gen_tcp:accept (state-socket state))))
    (logger:debug "connection accepted (server)")
    (lfe-ls-sup:start-socket)
    `#(noreply ,(set-state-socket state accept-socket))))

(defun double-nl () #"\r\n\r\n")

(defun %handle-new-req (rest-req req)
  (case (binary:split rest-req (double-nl))
    (`(,len ,rest)
     (let* ((len-and-nl (binary (len binary)
                                ((double-nl) binary)))
            (proper-rest (string:prefix rest-req
                                        len-and-nl)))
       (make-req expected-len (erlang:binary_to_integer len)
                 current-len (byte_size proper-rest)
                 data proper-rest)))))

(defun %received-handler (msg req)
  (logger:debug "Received msg len: ~p" `(,(byte_size msg)))
  (logger:debug "Received msg: ~p" `(,msg))
  (let ((new-req (case (string:prefix msg "Content-Length: ")
                   ('nomatch req);; append data to req data)
                   (rest-req (%handle-new-req rest-req req)))))
    `#(ok ,new-req)))

(defun handle_call
  ((`#(send ,msg) _from state)
   (logger:debug "Sending msg: ~s" `(,msg))
   (gen_tcp:send (state-socket state) (io_lib:format "~s~n" `(,msg)))
   `#(reply ok ,state))
  ((`#(received ,msg) _from state)
   (let* ((`#(,code ,new-req) (%received-handler msg (state-req state)))
          (new-state (set-state-req state new-req)))
     `#(reply #(,code ,new-state) ,new-state)))
  (('stop _from state)
   `#(stop normal state))
  ((message _from state)
   `#(reply ,(unknown-command) ,state)))

(defun handle_info
  ((`#(tcp ,socket ,(++ "quit" _)) state)
   (logger:debug "quit")
   (gen_tcp:close socket)
   `#(stop normal ,state))
  ((`#(tcp ,socket ,msg) state)
   (logger:debug "received msg len: ~p" `(,(byte_size msg)))
   (logger:debug "received msg: ~p" `(,msg))
   `#(noreply ,state))
  ((`#(tcp_error ,_socket ,_) state)
   (logger:debug "tcp-error")
   `#(stop normal ,state))
  ((`#(tcp_closed ,_socket) state)
   (logger:debug "tcp-closed")
   `#(stop normal ,state))
  
  ((`#(EXIT ,pid ,reason) state)
   (logger:debug "Process ~p exited! (Reason: ~p)~n" `(,pid ,reason))
   `#(noreply ,state))
  ((e state)
   (logger:warn "unexpected: ~p" `(,e))
   `#(noreply ,state)))

(defun terminate (_reason _state)
  'ok)

(defun code_change (_old-version state _extra)
  `#(ok ,state))

;;; private API

(defun concat-binary (bin1 bin2)
  (binary (bin1 binary) (bin2 binary)))

(defun binary-to-string (bin)
  (lists:flatten (io_lib:format "~p" `(,bin))))

;;; --------------
;;; our server API
;;; --------------

(defun pid ()
  (erlang:whereis (SERVER)))

(defun echo (msg)
  (gen_server:call (SERVER) `#(echo ,msg)))

(defun send (str)
  (gen_server:call (SERVER) `#(send ,str)))
