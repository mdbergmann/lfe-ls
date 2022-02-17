(defmodule lfe-ls-tcp-sup
  (beehaviour supervisor)
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
  (logger:debug "start_link (sup)")
  (supervisor:start_link `#(local ,(SERVER))
                         (MODULE)
                         (supervisor-opts)))

(defun stop ()
  (erlang:exit (pid) 'stopped))

;;; -----------------------
;;; callback implementation
;;; -----------------------

(defun init (_args)
  (logger:debug "init (sup)")
  (let (((tuple 'ok listen-socket) (gen_tcp:listen 10567 '(#(active false) binary))))
    (logger:debug "listen (sup) ok")
    ;;(spawn_link #'empty-listeners/0)
    `#(ok #(,(sup-flags)
            (,(child 'lfe-ls 'start_link `(,listen-socket)))))))

;;; -----------------
;;; public functions
;;; ----------------
(defun start-socket ()
  (logger:debug "start-socket (sup)")
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
