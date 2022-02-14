(defmodule completion-util
  (export all))

(include-lib "apps/lfe-ls/include/lsp-model.lfe")

(defun find-completions-at (text position trigger-char)
  (case trigger-char
    (#"(" (%find-global-and-local-symbols))))

(defun %find-global-and-local-symbols ()
  (lists:sort
   (lists:append (%predefined-lfe-functions)
                 (%predefined-erlang-functions))))

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
    (lists:map (lambda (ft)
                 (let ((`#(,name ,arity) ft))
                   (make-completion-item
                    label (erlang:list_to_binary
                           (io_lib:format "~p/~p" `(,name ,arity)))
                    kind (completion-item-kind-function)
                    detail detail)))
               visible-functions)))
