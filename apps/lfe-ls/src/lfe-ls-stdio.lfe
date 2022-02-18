(defmodule lfe-ls-stdio
  (behaviour gen_server)
  (export
   (start_link 1)
   (stop 0))
  (export
   (init 1)
   (handle_call 3)
   (handle_cast 2)
   (handle_info 2)
   (terminate 2)
   (code_change 3))
  (export
   (init-stdio-handler 1))
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

(defun start_link (io-device)
  (logger:info "start_link (lfe-ls-stdio)")  
  (gen_server:start_link `#(local ,(SERVER))
                         (MODULE)
                         `(#(device ,io-device))
                         (genserver-opts)))

(defun stop ()
  (gen_server:call (MODULE) 'stop))

;;; -----------------------
;;; callback implementation
;;; -----------------------

(defun init
  ((`(#(device ,io-device)))
   (logger:info "init (lfe-ls-stdio)")
   (logger:info "Spawning stdio handler...")
   (proc_lib:spawn_link (MODULE) 'init-stdio-handler `(#(device ,io-device)))   
   `#(ok 'null)))

(defun handle_call
  (('stop _from state)
   `#(stop normal state))
  ((message _from state)
   `#(reply #(error "Unknown command.") ,state)))

(defun handle_cast
  ((`#(send ,msg ,device) state)
   (case (file:write device msg)
     ('ok
      (logger:notice "Response written."))
     (`#(error ,reason)
      (logger:warning "Error on writing response: ~p" `(,reason))))
   `#(noreply state)))

(defun handle_info (info state)
  (logger:notice "Info: ~p" `(,info))
  `#(noreply state))

(defun terminate (_reason _state)
  (logger:warning "Terminated!")
  'ok)

(defun code_change (_old-version state _extra)
  `#(ok ,state))

;;; -----------------------
;;; server implementation
;;; -----------------------

(defun send (device msg)
  (logger:notice "Sending response...")
  (gen_server:cast (MODULE) `#(send ,msg ,device)))

(defun pid ()
  (erlang:whereis (SERVER)))

;;; -----------------------
;;; stdio handler
;;; -----------------------

(defun init-stdio-handler
  ((`#(device ,io-device))
   (logger:notice "init (stdio-handler)")
   (handler-loop '() (make-ls-state device io-device))))

(defun handler-loop (lines state)
  (let ((io-device (ls-state-device state)))
    (case (io:get_line io-device "")
      (#"\n"
       (logger:notice "lines: ~p" `(,lines))
       (let* ((headers (%parse-headers lines))
              (bin-len (proplists:get_value #"content-length" headers))
              (len (erlang:binary_to_integer bin-len))
              (`#(ok ,payload) (file:read io-device len))
              (new-state (%on-complete-req payload state)))
         (handler-loop '() new-state)))
      ('eof
       (logger:notice "eof"))
      (`#(error ,err)
       (logger:notice "error: ~p" `(,err)))
      (line
       (logger:notice "line: ~p" `(,line))
       (handler-loop (cons line lines) state))
      )))

(defun %on-complete-req (complete-req state)
  (logger:debug "Complete request: ~p" `(,complete-req))
  (logger:notice "Complete request of size: ~p" `(,(byte_size complete-req)))
  (let ((lsp-state (ls-state-lsp-state state))
        (io-device (ls-state-device state)))
    (case (lsp-proc:process-input complete-req lsp-state)
      (`#(#(reply ,lsp-proc-output) ,new-lsp-state)
       (logger:debug "lsp output: ~p" `(,lsp-proc-output))
       (response-sender:send-response (MODULE) io-device lsp-proc-output)
       (logger:debug "Response sent!")
       (clj:-> state
               (set-ls-state-req (make-req))
               (set-ls-state-lsp-state new-lsp-state)))
      (`#(#(noreply ,_) ,new-lsp-state)
       (logger:debug "lsp output with noreply")
       (clj:-> state
               (set-ls-state-req (make-req))
               (set-ls-state-lsp-state new-lsp-state))))))

(defun %parse-headers (lines)
  (lists:map #'%parse-header/1 lines))

(defun %parse-header (line)
  (let ((`(,name ,value) (binary:split line #":")))
    `#(,(string:trim (string:lowercase name)) ,(string:trim value))))
