(defun tcar (t)
  "First element in tuple."
  (cl:elt 0 t))

(defun tcdr (t)
  "Second element in tuple."
  (cl:elt 1 t))

(defun find-tkey (key lst)
  (cl:find-if (lambda (tup) (== key (tcar tup))) lst))

(defun concat-binary (bin1 bin2)
  (binary (bin1 binary) (bin2 binary)))

(defun binary-to-string (bin)
  (lists:flatten (io_lib:format "~p" `(,bin))))

(defun 1- (n) (- n 1))
(defun 1+ (n) (+ n 1))

(defun system-millis ()
  "Returns the current system millis as calculated from `os:timestamp`."
  (let (((tuple mega-secs secs micro-secs) (os:timestamp)))
    (+ (* mega-secs 1000000) (* secs 1000) (/ micro-secs 1000))))

(defun assert-cond (cond-fun max-millis)
  "Waits for the condition of `cond-fun` to be 'true.
Probes repeatedly until `max-millis`."
  (let ((start-millis (system-millis)))
    (fletrec ((floop ()
                     (let ((now-millis (system-millis)))
                       (if (> (- now-millis start-millis) max-millis)
                         'false
                         (let ((result (funcall cond-fun)))
                           (if result
                             'true
                             (progn
                               (timer:sleep 100)
                               (floop))))))))
      (floop))))
