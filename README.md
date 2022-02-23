# lfe-ls

*LFE Language Server implementation*

##### Table of Contents

* [About](#about-)
* [Build](#build-)
* [Start the Project REPL](#start-the-repl-)
* [Tests](#tests-)
* [Usage](#usage-)
* [License](#license-)

## About [&#x219F;](#table-of-contents)

LFE language server written in LFE.

This is largely work in progress. The following LSP functionality is implemented:

- `initialize` request/response
- `initialized` notification
- `textDocument/didOpen` notification which loads and maintains documents in memory.
- `textDocument/didChange` notification which updates the document in memory. Currently the sync transfers the full document. There is room form optimization here to implement a incremental sync.
- `textDocument/didClose` notification which deletes the document in memory.
- `textDocument/didSave` notification
- `textDocument/completion` request/response implements global symbols and loaded modules as well as module functions.

Planned further support is:

- hover support for showing documentation (via eldoc in Eglot).
- sending compiler diagnostics back to the LSP client.


## Build [&#x219F;](#table-of-contents)

Compile with:

```shell
$ rebar3 lfe compile
```

To create a release do:

```shell
$ rebar3 as prod do release -o <release-path>
```

An executable can be started as `release/lfe-ls/bin/lfe-ls` when `release-path` was just 'release' within the project.

# Start the Project REPL [&#x219F;](#table-of-contents)

```shell
$ rebar3 lfe repl
```

Starting the repl in this way wilkl automatically start the lfe-ls application and ls server.

# Tests [&#x219F;](#table-of-contents)

```shell
$ rebar3 as test eunit
```

## Usage [&#x219F;](#table-of-contents)

To use the lfe-ls server with Emacs Eglot you have to provide the host and port parameters. Those are 'localhost', port 10567.


## License [&#x219F;](#table-of-contents)

Apache License, Version 2.0

Copyright © 2022, Manfred Bergmann <mb@software-by-mabe.com>.
