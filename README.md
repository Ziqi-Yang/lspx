# LSPX

lspx allows you to easily organize different Emacs LSP clients (e.g. `eglot`, `lsp-mode`,
`lsp-bridge`).

It has built-in support for `eglot`, `lsp-mode` and `lsp-bridge`

## Installation

```emacs-lisp
(use-package lspx
  :ensure (:host codeberg :repo "meow_king/lspx")
  :config
  (lspx-setup-lspx))
```

## Usage

The principle of `lspx` follows the Eglot's design principle(better not use `add-hook`).
If you execute command `lspx`/`elgot` on a file in a project, then all files of the same type/major-mode
(e.g. `rust`) in this project will be managed by the same language server
(e.g. `rust-analyzer`), and you don't need to manually execute `lspx`/`eglot` again
on newly opened files of the same file type.

You can use `lspx` to choose and start a LSP client, use `lspx-shutdown` to shutdown
it.

Commands like `lspx-rename` are provided. They are basically glue functions
to automatically redirect to functions like `eglot-reanme` and `lsp-rename`.

Please check the source code for further information. It's a small file and can be
read without difficulty.
