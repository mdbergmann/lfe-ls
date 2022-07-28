(defmodule lfe-ls-tcp-sup
  (behaviour supervisor)
  ;; supervisor implementation
  (export
   (start_link 0)
   (stop 0))
  ;; callback implementation
  (export
   (init 1))
  ;; public implementation
  (export
   (start-socket 0)))

;;; ----------------
;;; config functions
;;; ----------------

(defun SERVER () (MODULE))
(defun supervisor-opts () '())
(defun sup-flags ()
  `#M(strategy one_for_one
      intensity 3
      period 60))

;;; -------------------------
;;; supervisor implementation
;;; -------------------------

(defun start_link ()
  (logger:debug "start_link (tcp-sup)")
  (supervisor:start_link `#(local ,(SERVER))
                         (MODULE)
                         (supervisor-opts)))

(defun stop ()
  (erlang:exit (pid) 'stopped))

;;; -----------------------
;;; callback implementation
;;; -----------------------

(defun init (_args)
  (logger:debug "init (tcp-sup)")
  (let* (((tuple 'ok port) (application:get_env 'lfe-ls 'port))
         ((tuple 'ok listen-socket) (gen_tcp:listen port '(#(active false) binary))))
    (logger:info "Starting on port: ~p" `(,port))
    (logger:debug "listen (tcp-sup) ok")
    `#(ok #(,(sup-flags)
            (,(child 'lfe-ls-tcp 'start_link `(,listen-socket)))))))

;;; -----------------
;;; public functions
;;; ----------------
(defun start-socket ()
  (logger:debug "start-socket (tcp-sup)")
  (supervisor:start_child (SERVER) '()))

;;; -----------------
;;; private functions
;;; -----------------

(defun pid ()
  (erlang:whereis (SERVER)))

(defun child (mod fun args)
  (logger:debug "mod: ~p, fun: ~p, args: ~p~n" `(,mod ,fun ,args))
  `#M(id ,mod
         start #(,mod ,fun ,args)
         restart permanent
         shutdown 2000
         type worker
         modules (,mod)))
