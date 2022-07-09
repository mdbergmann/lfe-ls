(defmodule utils
  (export (assert-cond 2)
          (system-millis 0)))

(defun system-millis ()
  (let (((tuple mega-secs secs micro-secs) (os:timestamp)))
    (+ (* mega-secs 1000000) (* secs 1000) (/ micro-secs 1000))))

(defun assert-cond (cond-fun max-millis)
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
