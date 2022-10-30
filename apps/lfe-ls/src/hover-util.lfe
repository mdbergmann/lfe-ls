(defmodule hover-util
  (export (get-docu 2)))

(include-lib "apps/lfe-ls/include/lsp-model.lfe")

(defun get-docu (text position)
  `#(ok ,(case (%parse-module-or-function text position)
           (`(,module ,func) (lfe_ls_docs:h (binary_to_atom module) (binary_to_atom func)))
           (`(,module) (lfe_ls_docs:h (binary_to_atom module))))))

(defun %parse-module-or-function (text position)
  "Parses the global symbol or function name, or the module if a ':' is part of the play.
Returns a list of one, or two entries.
One entry if there is no ':' in the text that was parsed.
Two entries if there was a ':' in the text that was parsed. The second element is empty if there is no text after the ':'. So generally the first element denotes a global symbol or function if only one element in the returned list. If two elements the first element is a module and the second element is either empty, or denotes a function in the module."
  (let* ((line-pos (position-line position))
         (char-pos (position-character position))
         (lines (string:split text #"\n" 'all))
         (line (cl:elt line-pos lines))
         (line-substr (%substr-to-next-space line char-pos))
         (tmp-mod (case `#(,(string:split line-substr #" " 'trailing)
                           ,(string:split line-substr #"(" 'trailing))
                    (`#((,_) (,_)) line-substr)
                    (`#((,_) (,_ ,b)) b)
                    (`#((,_ ,a) (,_)) a)
                    (`#((,_ ,a) (,_ ,b))
                     (if (< (string:length a)
                            (string:length b)) a b)))))
    (logger:debug "text: ~p" `(,text))
    (logger:debug "lines: ~p" `(,(length lines)))
    (logger:debug "line: ~p" `(,line))
    (logger:debug "line-pos: ~p" `(,line-pos))
    (logger:debug "line-substr: ~p, tmp-mod: ~p, char-pos: ~p" `(,line-substr ,tmp-mod ,char-pos))
    (string:split tmp-mod #":")))

(defun %substr-to-next-space (line char-pos)
  (let* ((string (binary_to_list line))
         (split-index (case
                          (lists:search (lambda (x)
                                          (let ((index (cl:elt 0 x))
                                                (value (cl:elt 1 x)))
                                            (and (> index char-pos)
                                                 (== value 32))))
                                 (lists:enumerate 1 string))
                        (`#(value ,val) (cl:elt 0 val))
                        ('false 0))))
    (list_to_binary
     (if (== split-index 0)
       ""
       (string:sub_string string 1 (- split-index 1))))))
    