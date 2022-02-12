(defmodule text-manip
  (export all))

(defun modify-text (text text-change)
                     ;; (let ((`#(#"range" ,range) (find-tkey #"range" change))
                     ;;       (`#(#"text" ,text) (find-tkey #"text" change)))
                     ;;   (logger:notice "range: ~p, text: ~p" `(,range ,text))
                     ;;   (let ((`#(#"start" ,start-pos) (find-tkey #"start" range))
                     ;;         (`#(#"end" ,end-pos) (find-tkey #"end" range)))
                     ;;     (logger:notice "start-pos: ~p, end-pos: ~p" `(,start-pos ,end-pos))
                     ;;     (let ((`#(#"line" ,start-line) (find-tkey #"line" start-pos))
                     ;;           (`#(#"character" ,start-char) (find-tkey #"character" start-pos))
                     ;;           (`#(#"line" ,end-line) (find-tkey #"line" end-pos))
                     ;;           (`#(#"character" ,end-char) (find-tkey #"character" end-pos)))
                     ;;       (logger:notice "sline: ~p, schar: ~p, eline: ~p, echar: ~p"
                     ;;                      `(,start-line ,start-char ,end-line ,end-char))
                     ;;       ;;do text manipulation
                           
                     ;;       ))))
  'null)
