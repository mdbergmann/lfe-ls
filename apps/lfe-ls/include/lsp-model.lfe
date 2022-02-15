
(defrecord lsp-state
  (initialized 'false (boolean))
  (documents #M()))

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

;; (defrecord range
;;   (start (make-position))
;;   (end (make-position)))

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
