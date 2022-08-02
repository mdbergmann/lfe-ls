(defmodule compile-util
  (export (compile-file 1)))

(include-lib "apps/lfe-ls/include/lsp-model.lfe")

(defun compile-file (path)
  "Compiles file at `path` and returns a list of `diagnostic-item`s."
  (logger:debug "cd: ~p" `(,(file:get_cwd)))
  (let ((comp-result (lfe_comp:file path '(verbose return))))
    (case comp-result
      ((tuple 'ok warnings _)
       (%generate-warn-diags warnings))
      ((tuple 'error '() errors _)
       (%generate-error-diags `(#(error ,errors ()))))
      ((tuple 'error errors _ _)
       (%generate-error-diags errors))
      (_
       comp-result))))

(defun %generate-warn-diags (warnings)
  "as in 3."
  (logger:debug "parsing diags: ~p" `(,warnings))
  (case warnings
    ('() #(ok ()))
    (`(#(ok ,module ()))
     #(ok ())) 
    (`(#(ok ,module (#(,file ,findings))))
     (let ((diags (lists:map (lambda (line)
                               (%generate-diag-entry 'warn line))
                             findings)))
       `#(ok ,diags)))))

(defun %generate-error-diags (errors)
  "as in 1."
  (logger:debug "parsing diags: ~p" `(,errors))
  (let ((`(#(error (#(,file ,findings)) ,_)) errors))
    (let ((diags (lists:map (lambda (line)
                              (%generate-diag-entry 'error line))
                            findings)))
      `#(ok ,diags))))

(defun %generate-diag-entry (severity line)
  "Handles a compile finding entry."
  (let ((`#(,line-num ,source ,message) line))
    (make-diagnostic-item
     range (line-to-range (- line-num 1))
     severity (atom-to-diag-severity severity)
     source (erlang:atom_to_binary source)
     message (erlang:list_to_binary
              (lists:flatten
               (lfe_io:format1 "~p" `(,message)))))))

#|
Compiling a file with setting include search path.

1. undefined function export
#(error
  (#(error
     (#("/Users/mbergmann/Development/MySources/lfe-ls/apps/lfe-ls/test/compile-tmpls/error-no-include.lfe"
        (#(1 lfe_lint #(undefined_function #(my-fun 1))))))
     ()))
  () ())

2. include doesn't exist
#(error ()
  (#("/Users/mbergmann/Development/MySources/lfe-ls/compile-tmpls/error-incl-not-exists.lfe"
     (#(3 lfe_macro_include #(no_include lib "doesnt-exist.lfe")))))
  ())

3.
(lfe_comp:file "/Users/mbergmann/Development/MySources/lfe-ls/apps/lfe-ls/src/completion-util.lfe"
lfe>                     '(verbose return #(i "/Users/mbergmann/Development/MySources/lfe-ls")))
#(ok
  (#(ok completion-util
     (#("/Users/mbergmann/Development/MySources/lfe-ls/apps/lfe-ls/src/completion-util.lfe"
        (#(4 erl_lint #(unused_function #(1- 1)))
         #(4 erl_lint #(unused_function #(binary-to-string 1)))
         #(4 erl_lint #(unused_function #(concat-binary 2)))
         #(4 erl_lint #(unused_function #(find-tkey 2)))
         #(4 erl_lint #(unused_function #(tcar 1)))
         #(4 erl_lint #(unused_function #(tcdr 1)))
         #(5 erl_lint #(unused_function #(completion-item-kind-struct 0)))
         #(5 erl_lint #(unused_function #(completion-item-kind-variable 0)))
         #(5 erl_lint #(unused_function #(req-invalid-request-error 0)))
         #(5 erl_lint #(unused_function #(req-parse-error 0)))
         #(5 erl_lint #(unused_function #(trigger-kind-character 0)))
         #(5 erl_lint #(unused_function #(trigger-kind-invoked 0)))
         #(5 erl_lint #(unused_record document))
         #(5 erl_lint #(unused_record lsp-state)))))))
           I
         is line  
())

4. (TODO)
[{error,[{"/foobar.lfe",[{none,file,enoent}]}],[]}]

TODO:
- how to determine source and include folders in a project

|#