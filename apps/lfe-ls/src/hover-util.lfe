(defmodule hover-util
  (export (get-docu 2)))

(include-lib "apps/lfe-ls/include/lsp-model.lfe")

(defun get-docu (text position)
  "Retrieves the Erlang EEP-48 documentation (if present) for the recognized token in `text` at `position`.
The return is (tuple 'ok binary) for success, or (tuple 'error reason) on error."
  (case
      (case (%parse-module-or-function text position)
        ('(#"") #"")
        (`(,module ,func) (lfe_ls_docs:h (binary_to_atom module) (binary_to_atom func)))
        (`(,module) (lfe_ls_docs:h (binary_to_atom module))))
    (`#(error ,err) `#(error ,err))
    (doc `#(ok ,doc))))

(defun %parse-module-or-function (text position)
  "Parses the function name, or the module if a ':' is part of the play.
Returns a list of one, or two entries.
One entry if there is no ':' in the text that was parsed.
Two entries if there was a ':' in the text that was parsed. The second element is empty if there is no text after the ':'. So generally the first element denotes a global symbol or function if only one element in the returned list. If two elements the first element is a module and the second element is either empty, or denotes a function in the module."
  (let* ((line-pos (position-line position))
         (char-pos (position-character position))
         (lines (string:split text #"\n" 'all))
         (line (cl:elt line-pos lines))
         (line-substr (%substr-to-right-delim line char-pos))
         (captured-token (%substr-to-left-delim line-substr))
         (token-start-pos (%find-token-start-pos line captured-token)))
    ;; (logger:notice "text: ~p" `(,text))
    ;; (logger:notice "lines: ~p" `(,(length lines)))
    ;; (logger:notice "line: ~p" `(,line))
    ;; (logger:notice "line-pos: ~p" `(,line-pos))
    ;; (logger:notice "line-substr: ~p, captured-token: ~p, char-pos: ~p" `(,line-substr ,captured-token ,char-pos))
    (cond
     ((< char-pos token-start-pos) '(#""))
     (else (string:split captured-token #":")))))

(defun %substr-to-right-delim (line char-pos)
  "Makes a sub-string from 0 to the next right side delimiter whose index is > `char-pos`.
Delimiters are:  ' ' or ')'
I.e. line = `  (io:format \"foo\" '())`
Then this function returns: `  (io:format`"
  (let* ((string (binary_to_list line))
         (split-index (case
                          (lists:search (lambda (x)
                                          (let ((index (cl:elt 0 x))
                                                (value (cl:elt 1 x)))
                                            (and (> index char-pos)
                                                 (or (== value (hd " "))
                                                     (== value (hd ")"))))))
                                 (lists:enumerate 1 string))
                        (`#(value ,val) (cl:elt 0 val))
                        (_ 'not-found))))
    (list_to_binary
     (if (== split-index 'not-found)
       (string:sub_string string 1)
       (string:sub_string string 1 (- split-index 1))))))

(defun %substr-to-left-delim (line-substr)
  (case `#(,(string:split line-substr #" " 'trailing)
           ,(string:split line-substr #"(" 'trailing))
    (`#((,_) (,_)) line-substr)
    (`#((,_) (,_ ,b)) b)
    (`#((,_ ,a) (,_)) a)
    (`#((,_ ,a) (,_ ,b))
     (if (< (string:length a) (string:length b))
       a b))))

(defun %find-token-start-pos (line token)
  (let ((prefix (case (string:split line token 'leading)
                  (`(,prefix ,_) prefix)
                  (`(,prefix) prefix))))
    (string:length prefix)))
