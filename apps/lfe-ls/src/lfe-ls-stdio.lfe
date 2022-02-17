(defmodule lfe-ls-stdio
  (export all))

(include-lib "apps/lfe-ls/include/utils.lfe")
(include-lib "apps/lfe-ls/include/ls-model.lfe")

(defun spawn_link (io-device)
  (logger:info "spawn_link (lfe-ls-stdio)")
  (proc_lib:spawn_link (MODULE) 'init `(#(device ,io-device))))

(defun init
  ((`#(device ,io-device))
   (logger:info "init (lfe-ls-stdio)")
   ;;(io:setopts io-device '(binary #(encoding latin1)))
   (loop '() (make-ls-state device io-device))))

(defun loop (lines state)
  (let ((io-device (ls-state-device state)))
    (case (io:get_line io-device "")
      (#"\n"
       (logger:notice "lines: ~p" `(,lines))
       (let* ((headers (%parse-headers lines))
              (bin-len (proplists:get_value #"content-length" headers))
              (len (erlang:binary_to_integer bin-len))
              (`#(ok ,payload) (file:read io-device len)))
         (let ((new-state (%on-complete-req payload state)))
           (loop '() new-state))))
      ('eof
       (logger:notice "eof"))
      (`#(error ,err)
       (logger:notice "error: ~p" `(,err)))
      (line
       (logger:notice "line: ~p" `(,line))
       (loop (cons line lines) state))
      )))

(defun %on-complete-req (complete-req state)
  (logger:debug "Complete request: ~p" `(,complete-req))
  (logger:notice "Complete request of size: ~p" `(,(byte_size complete-req)))
  (let ((lsp-state (ls-state-lsp-state state))
        (io-device (ls-state-device state)))
    (case (lsp-proc:process-input complete-req lsp-state)
      ;; probably we have more cases
      (`#(#(reply ,lsp-proc-output) ,new-lsp-state)
       (logger:debug "lsp output: ~p" `(,lsp-proc-output))
       (response-sender:send-response io-device lsp-proc-output)
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

(defun send (device msg)
  (io:format device "~s" `(,msg)))
