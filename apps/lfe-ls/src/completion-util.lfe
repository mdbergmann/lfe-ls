(defmodule completion-util
  (export (find-completions-at 3)))

(include-lib "apps/lfe-ls/include/utils.lfe")
(include-lib "apps/lfe-ls/include/lsp-model.lfe")

(defun find-completions-at (text position trigger-char)
  (lists:sort
   (case trigger-char
     (#"(" (%find-symbols-and-modules))
     (#":" (%find-module-functions
            (%parse-module-or-symbol-name-backwards
             text position)))
     (_ (%find-symbols-and-modules-or-module-functions
         text position)))))

(defun %find-symbols-and-modules-or-module-functions (text position)
  (let ((module-or-symbol (%parse-module-or-symbol-name-backwards text position)))
    (case module-or-symbol
      (`(,a ,_) (%find-module-functions `(,a)))
      (`(,a) (%find-symbols-and-modules)))))

(defun %parse-module-or-symbol-name-backwards (text position)
  (let* ((line-pos (position-line position))
         (char-pos (position-character position))
         (lines (string:split text #"\n" 'all))
         (line (cl:elt line-pos lines))
         (line-substr (string:slice line 0 char-pos))
         (tmp-mod (case `#(,(string:split line-substr #" " 'trailing)
                           ,(string:split line-substr #"(" 'trailing))
                    (`#((,_) (,_)) line-substr)
                    (`#((,_) (,_ ,b)) b)
                    (`#((,_ ,a) (,_)) a)
                    (`#((,_ ,a) (,_ ,b))
                     (if (< (string:length a)
                            (string:length b)) a b)))))
    ;; (logger:notice "text: ~p" `(,text))
    ;; (logger:notice "lines: ~p" `(,(length lines)))
    ;; (logger:notice "line: ~p" `(,line))
    ;; (logger:notice "line-pos: ~p" `(,line-pos))
    ;; (logger:notice "line-substr: ~p, tmp-mod: ~p, char-pos: ~p" `(,line-substr ,tmp-mod ,char-pos))
    (string:split tmp-mod #":")))

(defun %find-module-functions
  (('()) '())
  ((module)
   ;;(logger:notice "module: ~p" `(,module))
   (let* ((module-name (case module
                         (`(,a ,_) a)
                         (`(,a) a)))
          (module-info (call (erlang:binary_to_atom module-name) 'module_info))
          (module-funs (cl:elt 1 (cadr module-info))))
     (%fun-tuples-to-completions module-funs (binary (module-name binary) (#":" binary))))))

(defun %find-symbols-and-modules ()
  (lists:append `(,(%predefined-lfe-functions)
                  ,(%predefined-erlang-functions)
                  ,(%loaded-modules))))

(defun %predefined-erlang-functions ()
  (%prep-internal-functions (erlang:module_info)
                            (lambda (name arity)
                              (erl_internal:bif name arity))
                            #"erlang:"))

(defun %predefined-lfe-functions ()
  "Predefined LFE functions."
  (%prep-internal-functions (lfe:module_info)
                            (lambda (name arity)
                              (lfe_internal:is_lfe_bif name arity))
                            #"lfe:"))

(defun %loaded-modules ()
  (lists:map (lambda (m)
               (let ((module-name (cl:elt 0 m)))
                 (make-completion-item
                  label (erlang:atom_to_binary module-name)
                  kind (completion-item-kind-module))))
             (code:all_loaded)))

(defun %prep-internal-functions (module-info bif-fun-pred detail)
  (let* ((mod-functions (cl:elt 1 (cadr module-info)))
         (visible-functions (lists:filter (lambda (ft)
                                            (let ((`#(,name ,arity) ft))
                                              (funcall bif-fun-pred name arity)))
                                          mod-functions)))
    (%fun-tuples-to-completions visible-functions detail)))

(defun %fun-tuples-to-completions (ftuples detail)
  (lists:map (lambda (ft)
               (let* ((`#(,name ,arity) ft)
                      (fun-name (erlang:atom_to_list name))
                      (fun-name-bin (erlang:atom_to_binary name)))
                 (make-completion-item
                  label (erlang:list_to_binary
                         (io_lib:format "~s/~p" `(,fun-name ,arity)))
                  kind (completion-item-kind-function)
                  detail detail
                  insert-text fun-name-bin)))
             ftuples))
