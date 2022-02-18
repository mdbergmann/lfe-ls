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
  ;;(lfe-ls-sup:start_link)
  (io:setopts 'standard_io '(binary #(encoding latin1)))
  (lfe-ls-stdio:spawn_link 'standard_io)
  )

(defun stop ()
  ;;(lfe-ls-sup:stop)
  (lfe-ls-stdio:stop)
  'ok)
