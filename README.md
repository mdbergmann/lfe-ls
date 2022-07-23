# lfe-ls

*LFE Language Server implementation*

## About [&#x219F;](#table-of-contents)

LFE language server written in LFE.

I started this project because I wanted to try LFE in a real project and LFE has a bit of a lack of tooling.  
It is largely work in progress. The following LSP functionality is implemented:

- `initialize` request/response
- `initialized` notification
- `textDocument/didOpen` notification which loads and maintains documents in memory.
- `textDocument/didChange` notification which updates the document in memory. Currently the sync transfers the full document. There is room form optimization here to implement a incremental sync.
- `textDocument/didClose` notification which deletes the document in memory.
- `textDocument/didSave` notification
- `textDocument/completion` request/response implements global symbols and loaded modules as well as module functions.
- `textDocument/publishDiagnostics` notification to push compile diagnostics on saving a file.

Planned further support is:

- hover support for showing documentation (via eldoc in Eglot).

Auto-completion is rudimentary right now. It completes global symbols/atoms, functions/macros and functions on a module basis by parsing the text and looking for ':' character which indicates completing for a module.
There is a lot of room for improvement. I.e.: functions within a module could be parsed from the text, or variables within a let.


## Build [&#x219F;](#table-of-contents)

Compile with:

```shell
$ rebar3 lfe compile
```

To create a release do:

```shell
$ rebar3 as prod do release,escriptize
```

An executable can be started as `_build/prod/bin/lfe-ls`.

## Eglot for Emacs:

To add lfe-ls as a server in Eglot the following can be added to the Eglot config:

```
(add-to-list
   'eglot-server-programs
   '(lfe-mode . ("/path/to/lfe-ls/_build/prod/bin/lfe-ls"
                 "--transport" "tcp" "--port" :autoport)))
```

# Start the Project REPL [&#x219F;](#table-of-contents)

```shell
$ rebar3 lfe repl
```

Starting the repl in this way wilkl automatically start the lfe-ls application and ls server.

# Tests [&#x219F;](#table-of-contents)

```shell
$ rebar3 as test eunit
```

## License [&#x219F;](#table-of-contents)

Apache License, Version 2.0

Copyright © 2022, Manfred Bergmann <mb@software-by-mabe.com>.
