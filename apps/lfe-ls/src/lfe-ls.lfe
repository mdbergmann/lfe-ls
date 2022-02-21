(defmodule lfe-ls
  (export (main 1)))

(defun main(args)
  (io:format "Args: ~p~n" `(,args))
  (application:load 'getopt)
  (application:load 'lfs-le)
  (let (('ok (parse-args args)))
    (io:format "port: ~p~n" `(,(application:get_env 'lfe-ls 'port)))
    (configure-logging)
    (case (application:ensure_all_started 'lfe-ls)
      ((tuple 'ok _)
       (io:format "App started.~n")
       (logger:info "Application started.")
       (receive
         (_ 'ok)))
      (_
       (io:format "Starting app failed!~n")))))

(defun parse-args (args)
  (case (getopt:parse (args-spec-list) args)
    ((tuple 'ok `#(,parsed-args ,_bad-args))
     (set-args parsed-args))
    ((tuple 'error `#(invalid_option _))
     (getopt:usage (args-spec-list) "lfs-ls")
     (halt 1))))

(defun args-spec-list ()
  '(#(port #\p "port" #(integer 10567) "Port for tcp server (default 10567)")
    #(transport #\t "transport" #(string "stdio") "The transport to be used (tcp (default) | stdio)")))



(defun set-args
  (('()) 'ok)
  (((cons `#(port ,val) rest))
   (application:set_env 'lfe-ls 'port val)
   (set-args rest))
  (((cons `#(transport ,val) rest))
   (application:set_env 'lfe-ls 'transport val)
   (set-args rest)))

(defun configure-logging ()
  (let* ((log-dir (filename:basedir 'user_log "lfe-ls"))
         (log-file (filename:join log-dir "lfe-ls.log"))
         ('ok (filelib:ensure_dir log-dir))
         (handler `#M(config #M(file ,log-file)
                             level debug
                             formatter #(logger_formatter #M(single_line true)))))
    (io:format "Logging to: ~p~n" `(,log-file))
    (lists:foreach (lambda (id)
                     (io:format "Removing logger: ~p~n" `(,id))
                     (logger:remove_handler id))
                   (logger:get_handler_ids))
    (logger:add_handler 'lfe-ls-handler 'logger_std_h handler)
    (logger:set_primary_config 'level 'debug)))
