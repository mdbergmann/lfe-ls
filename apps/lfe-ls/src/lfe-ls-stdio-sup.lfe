(defmodule lfe-ls-stdio-sup
  (beehaviour supervisor)
  ;; supervisor implementation
  (export
   (start_link 0)
   (stop 0))
  ;; callback implementation
  (export
   (init 1))
  (export
   (start-child 0)))

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
  (logger:debug "start_link (stdio-sup)")
  (supervisor:start_link `#(local ,(SERVER))
                         (MODULE)
                         (supervisor-opts)))

(defun stop ()
  (erlang:exit (pid) 'stopped))

;;; -----------------------
;;; callback implementation
;;; -----------------------

(defun init (_args)
  (logger:debug "init (stdio-sup)")
  ;;(let ((io-device (restrict-stdio-access)))
  (io:setopts 'standard_io '(binary))
  `#(ok #(,(sup-flags)
          (,(child 'lfe-ls-stdio 'start_link `(standard_io))))))

;;; -----------------
;;; public functions
;;; ----------------
(defun start-child ()
  (logger:debug "start-child (stdio-sup)")
  (supervisor:start_child (SERVER) '()))

;;; -----------------
;;; private functions
;;; -----------------

(defun restrict-stdio-access ()
  "Restrict access to standard I/O

Sets the `io_device' application variable to the current group
leaders and replaces the group leader process of this supervisor,
for a fake one. This fake group leader is propagated to all of this
supervisor's children.

This prevents any library that decides to write anything to
standard output from corrupting the messages sent through JSONRPC.
This problem is happening for example when calling `edoc:get_doc/2',
which can print warnings to standard output."
  (logger:info "Use group leader as io-device")
  (logger:info "Replace group leader to avoid unwanted output to stdout,")

  (let ((pid (erlang:spawn #'noop-group-leader/0)))
    (erlang:group_leader pid (self)))
  (erlang:group_leader))

(defun noop-group-leader ()
  (receive
    (msg
     (logger:info "noop-group-leader got message: ~p" `(,msg))
     (case msg
       (`#(io_request ,from ,reply-as getopts)
        (! from #(io_reply reply-as (io:getopts))))
       (`#(io_request ,from ,reply-as ,_)
        (! from `#(io_reply reply-as ok)))
       (_
        'ok))
     (noop-group-leader))))

(defun pid ()
  (erlang:whereis (SERVER)))

(defun child (mod fun args)
  (logger:debug "mod: ~p, fun: ~p, args: ~p~n" `(,mod ,fun ,args))
  `#M(id ,mod
         start #(,mod ,fun ,args)
         type worker))
