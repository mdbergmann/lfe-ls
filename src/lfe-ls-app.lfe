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
  (lfe-ls-sup:start_link))

(defun stop ()
  (lfe-ls-sup:stop)
  'ok)
