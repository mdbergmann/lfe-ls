(defmodule lfe-ls-app
  (behaviour application)
  ;; app implementation
  (export
   (start 2)
   (stop 0)))

;;; --------------------------
;;; application implementation
;;; --------------------------

(defun start (_type _args)
  (logger:set_application_level 'lfe-ls 'all)
  (logger:info "Starting lfe-ls application ...")
  (case (application:get_env 'lfe-ls 'transport)
    (`#(ok "stdio")
     (lfe-ls-stdio-sup:start_link))
    (`#(ok "tcp")
     (lfe-ls-tcp-sup:start_link))
    (_
     (logger:error "Unknown transport!"))))

(defun stop ()
  (case (application:get_env 'lfe-ls 'transport)
    (`#(ok "stdio")
     (lfe-ls-stdio-sup:stop))
    (`#(ok "tcp")
     (lfe-ls-tcp-sup:stop))
    (_
     (logger:error "Unknown transport!"))))
