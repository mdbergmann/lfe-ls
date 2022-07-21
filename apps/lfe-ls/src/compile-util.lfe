(defmodule compile-util
  (export (compile-file 1)))

(defun compile-file (path)
  (let ((comp-result (lfe_comp:file path '(verbose return))))
    (case comp-result
      ((tuple ok _ _) #(ok ())))))


#|
Compiling a file with setting include search path.

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

lfe> (lfe_comp:file "/Users/mbergmann/Development/MySources/lfe-ls/apps/lfe-ls/src/lsp-proc.lfe" '(verbose return)) 
#(error ()
  (#("/Users/mbergmann/Development/MySources/lfe-ls/apps/lfe-ls/src/lsp-proc.lfe"
     (#(5 lfe_macro_include
        #(no_include lib "apps/lfe-ls/include/utils.lfe")))))
  ())


TODO:
- how to determine source and include folders in a project

|#