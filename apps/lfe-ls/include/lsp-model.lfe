
(defrecord lsp-state
  (initialized 'false (boolean))
  (documents #M()))

(defun req-parse-error () -32700)
(defun req-invalid-request-error () -32600)

(defrecord document
  (uri #"" (binary))
  (version 0 (integer))
  (text #"" (binary)))

(defrecord position
  (line 0 (integer))
  (character 0 (integer)))

(defun trigger-kind-invoked () 0)
(defun trigger-kind-character () 1)

(defun completion-item-kind-macro () 2)  ; reused for macro
(defun completion-item-kind-function () 3)
(defun completion-item-kind-variable () 6)
(defun completion-item-kind-module () 9)
(defun completion-item-kind-keyword () 14)
(defun completion-item-kind-struct () 22)

(defrecord completion-item
  (label)
  (kind)
  (detail #"" (binary))
  (insert-text #"" (binary)))

(defrecord range
  (start (make-position))
  (end (make-position)))

(defun line->range (line-num)
  (make-range start (make-position line line-num)
              end (make-position line line-num)))

(defun diag-severity-error () 1)
(defun diag-severity-warn () 2)
(defun diag-severity-info () 3)
(defun diag-severity-hint () 4)

(defun atom->diag-severity (atom)
  (case atom
    ('error 1)
    (_ 4)))

(defrecord diagnostic-item
  (range)
  (severity)
  (source #"" (binary))
  (message #"" (binary)))

;; (defrecord text-change
;;   (range (make-range))
;;   (text #"" (binary)))

;; (defun change-to-text-change-rec (change)
;;   (let ((`#(#"range" ,range) (find-tkey #"range" change))
;;         (`#(#"text" ,text) (find-tkey #"text" change)))
;;     (let ((`#(#"start" ,start-pos) (find-tkey #"start" range))
;;           (`#(#"end" ,end-pos) (find-tkey #"end" range)))
;;       (let ((`#(#"line" ,start-line) (find-tkey #"line" start-pos))
;;             (`#(#"character" ,start-char) (find-tkey #"character" start-pos))
;;             (`#(#"line" ,end-line) (find-tkey #"line" end-pos))
;;             (`#(#"character" ,end-char) (find-tkey #"character" end-pos)))
;;         (make-text-change range (make-range start (make-position line start-line
;;                                                                  character start-char)
;;                                             end (make-position line end-line
;;                                                                character end-char))
;;                           text text)))))
