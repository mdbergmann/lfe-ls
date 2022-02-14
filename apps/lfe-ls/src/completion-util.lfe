(defmodule completion-util
  (export (find-completions-at 3)))

(include-lib "apps/lfe-ls/include/lsp-model.lfe")

(defun find-completions-at (text position trigger-char)
  (lists:sort
   (case trigger-char
     (#"(" (%find-global-and-local-symbols))
     (#":" (%find-module-functions
            (%parse-module-name-backwards text
                                          (set-position-character position
                                           (1- (position-character position)))))))))

(defun 1- (n)
  (- n 1))

(defun %parse-module-name-backwards (text position)
  (let* ((line (position-line position))
         (char-pos (position-character position))
         (lines (binary:split text #"\n"))
         (line (cl:elt line lines))
         (tmp-mod (case `#(,(string:split line #" " 'trailing) ,(string:split line #"(" 'trailing))
                    (`#((,_) (,_)) line)
                    (`#((,_) (,_ ,b)) b)
                    (`#((,_ ,a) (,_)) a)
                    (`#((,_ ,a) (,_ ,b)) (if (< (string:length a)
                                                (string:length b)) a b)))))
    (case (string:split tmp-mod #":")
      (`(,a) a)
      (`(,a ,_) a))))

(defun %find-module-functions (module)
  (let* ((module-info (call (erlang:binary_to_atom module) 'module_info))
         (module-funs (cl:elt 1 (cadr module-info))))
    (%fun-tuples-to-completions module-funs (binary (module binary) (#":" binary)))
  ))

(defun %find-global-and-local-symbols ()
  (lists:append (%predefined-lfe-functions)
                (%predefined-erlang-functions)))

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

(defun %prep-internal-functions (module-info bif-fun-pred detail)
  (let* ((mod-functions (cl:elt 1 (cadr module-info)))
         (visible-functions (lists:filter (lambda (ft)
                                            (let ((`#(,name ,arity) ft))
                                              (funcall bif-fun-pred name arity)))
                                          mod-functions)))
    (%fun-tuples-to-completions visible-functions detail)))

(defun %fun-tuples-to-completions (ftuples detail)
  (lists:map (lambda (ft)
               (let ((`#(,name ,arity) ft))
                 (make-completion-item
                  label (erlang:list_to_binary
                         (io_lib:format "~p/~p" `(,name ,arity)))
                  kind (completion-item-kind-function)
                  detail detail)))
             ftuples))
