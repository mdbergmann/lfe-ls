(defmodule code-util
  (export
   (add-code-paths 1)))

(defun add-code-paths (project-root-path)
  "Add code paths for all files under
'rootpath/_build/default/lib/*/ebin' + 'rootpath/_build/test/lib/*/ebin'
for folder that are no symlinks"
  (flet ((collect-dirs (root-dir)
                       (case (file:list_dir root-dir)
                         (`#(ok ,dirs)
                          (lists:map (lambda (dir)
                                       (filename:join root-dir dir)) dirs))
                         (`#(error ,err)
                          (logger:warning "Collecting folders: ~p" `(,err))
                          '()))))
    
    (let* ((libs-root (filename:join project-root-path "_build/default/lib"))
           (test-libs-root (filename:join project-root-path "_build/test/lib"))
           (all-libs-dirs (lists:append
                                 (collect-dirs libs-root)
                                 (collect-dirs test-libs-root)))
           (final-dirs (clj:->> all-libs-dirs
                            (lists:filter #'filelib:is_dir/1)
                            (lists:map (lambda (dir) (filename:join dir "ebin")))
                            (lists:append `(,(filename:join `(,project-root-path ".lfe-ls-out")))))))
      (lists:foreach (lambda (dir)
                       (case (code:add_patha (binary_to_list dir))
                         (`#(error ,err)
                          (logger:warning "Can't load path: ~p, err: ~p" `(,dir ,err)))
                         (_
                          (logger:info "Path loaded: ~p" `(,dir)))))
             final-dirs))))
