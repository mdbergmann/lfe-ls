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
     (%fun-tuples-to-completions module-funs
                                 (completion-item-kind-function)
                                 (binary (module-name binary) (#":" binary))))))

(defun %find-symbols-and-modules ()
  (lists:append `(,(%predefined-lfe-functions)
                  ,(%predefined-erlang-functions)
                  ,(%predefined-lfe-core-forms)
                  ,(%predefined-macro-forms)
                  ,(%predefined-common-lisp-inspired-macros)
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

(defun %predefined-lfe-core-forms ()
  (%fun-tuples-to-completions
   (lists:map (lambda (elem)
                `#(,elem null))
              '(quote cons car cdr list tuple binary map map-get map-set map-update lambda
                      match-lambda let let-function letrec-function let-macro progn if case receive
                      catch try case catch when after funcall call define-module extend-module
                      define-function define-macro type-test guard-bif
                      include-lib))
   (completion-item-kind-keyword)
   #"lfe-core"))

(defun %predefined-macro-forms ()
  (%fun-tuples-to-completions
   (lists:map (lambda (elem)
                `#(,elem null))
              '(list* let* flet flet* fletrec cond andalso orelse fun fun lc list-comp
                      bc binary-comp match-spec))
   (completion-item-kind-macro)
   #"lfe-core"))

(defun %predefined-common-lisp-inspired-macros ()
  (%fun-tuples-to-completions
   (lists:map (lambda (elem)
                `#(,elem null))
              '(defun defmacro defsyntax macrolet syntaxlet prog1 prog2 defmodule defrecord))
   (completion-item-kind-keyword)
   #"lfe-core"))

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
    (%fun-tuples-to-completions visible-functions
                                (completion-item-kind-function)
                                detail)))

(defun %fun-tuples-to-completions (ftuples kind detail)
  (lists:map (lambda (ft)
               (let* ((`#(,name ,arity) ft)
                      (fun-name (erlang:atom_to_list name))
                      (fun-name-bin (erlang:atom_to_binary name)))
                 (make-completion-item
                  label (case arity
                          ('null fun-name-bin)
                          (ar (erlang:list_to_binary
                               (io_lib:format "~s/~p" `(,fun-name ,ar)))))
                  kind kind
                  detail detail
                  insert-text fun-name-bin)))
             ftuples))
