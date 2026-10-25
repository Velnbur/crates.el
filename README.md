# crates.el

[![CI](https://github.com/Velnbur/crates.el/actions/workflows/ci.yml/badge.svg)](https://github.com/Velnbur/crates.el/actions/workflows/ci.yml)

Show the latest crates.io version of every dependency in a `Cargo.toml`,
right next to it, without a network round-trip per crate.

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }   ✓ 1.0.229
tokio = "1.2"                                        ↑ 1.54.1
rand = "0.8"                                         → 0.10.3
```

- `✓` the requirement names the latest version
- `↑` the requirement allows a newer version than it names
- `→` the requirement excludes the latest version
- `✗` unknown crate, no matching version, or only yanked ones match

Versions come from a local SQLite database built from the official
[crates.io database dump](https://crates.io/data-access#database-dumps),
so lookups are instant and work offline.

## Requirements

- Emacs 29.1 or newer, built with SQLite and tree-sitter support
- the `toml` tree-sitter grammar (`M-x treesit-install-language-grammar`)
- `curl`, `tar` (GNU or bsd) and `sqlite3` (3.32+) on `PATH`, to build the database

## Setup

```elisp
(use-package crates
  :vc (:url "https://github.com/Velnbur/crates.el")
  :mode ("/Cargo\\.toml\\'" . crates-mode))
```

Then build the database once:

    M-x crates-db-update

This downloads the daily dump (about 2 GB) and needs a few GB of free
space in `temporary-file-directory` while it runs. It runs in the
background; progress is logged in `*crates-db-update*`. The resulting
database is a small fraction of that and lives at `crates-db-file`.

To refresh it automatically when it is older than `crates-db-max-age`
days (7 by default), set `crates-db-auto-update` to `t`.

## Usage

`crates-mode` is derived from `toml-ts-mode` and annotates the buffer as
you edit. Hover an annotation for details.

All commands live under the `C-c c` prefix (`crates-command-map`):

| Key       | Command                  |                                                         |
|-----------|--------------------------|---------------------------------------------------------|
| `C-c c u` | `crates-bump-at-point`   | Set the requirement at point to the latest version      |
| `C-c c a` | `crates-bump-all`        | Bump every bare/`^`/`~` requirement (`C-u`: only outdated ones) |
| `C-c c e` | `crates-expand-at-point` | Turn `name = "1"` into `name = { version = "1" }`       |
| `C-c c o` | `crates-browse`          | Open the crate's crates.io page                         |
| `C-c c r` | `crates-refresh`         | Re-check the buffer                                     |
| `C-c c U` | `crates-db-update`       | Rebuild the local crates.io database                    |

To use a different prefix, rebind the map, e.g.
`(keymap-set crates-mode-map "C-c C-c" 'crates-command-map)`.

Bumping keeps the operator and precision you wrote: `"1.2"` becomes
`"1.45"`, `"~0.3.1"` becomes `"~0.4.2"`.

### Completion

Two `completion-at-point` functions are installed, so they work with
the default `C-M-i`, `company-mode` (through `company-capf`), Corfu
and friends:

- **Crate names**, on a key in a dependencies table or in a
  `[dependencies.NAME]` header. Candidates are ordered by downloads and
  annotated with their latest version and description:

  ```
  [dependencies]
  ser|
      serde             1.0.229   A generic serialization/deserialization framework
      serde_derive      1.0.229   Macros 1.1 implementation of #[derive(Serialize, Deserializ…
      serde_json        1.0.154   A JSON serialization file format
      serde_core        1.0.229   Serde traits only, with no support for derive -- use the `s…
      serde_spanned     1.1.2     Serde-compatible spanned Value
  ```

  With company, the description is also shown in the echo area, and
  `<f1>` on a candidate opens its full description, download count and
  crates.io link. Choosing a name on an empty line also writes its
  version (`serde = "1.0.229"`); turn that off with
  `crates-name-completion-insert-version`. Case and `-`/`_` are ignored
  when matching, as on crates.io.

  In a Cargo workspace, a crate that the root manifest already declares
  in `[workspace.dependencies]` is inherited instead:
  `serde = { workspace = true }`. Set
  `crates-name-completion-workspace` to `dotted` for
  `serde.workspace = true`, or to `nil` to always write a version.
  The option is safe as a directory local, so each project can choose
  its own style in `.dir-locals.el`:

  ```elisp
  ((crates-mode . ((crates-name-completion-workspace . dotted))))
  ```

- **Versions**, inside a requirement string: the crate's non-yanked
  versions, newest first, with their release dates.

Dependencies are recognised in `[dependencies]`, `[dev-dependencies]`,
`[build-dependencies]`, `[workspace.dependencies]` and
`[target.'cfg(...)'.dependencies]`, written as strings, inline tables,
dotted keys or sub-tables. Renamed dependencies (`package = "..."`) are
looked up by their real name; `workspace = true` entries and other
registries are skipped.

## Customization

`M-x customize-group RET crates`. Notable options: `crates-db-file`,
`crates-db-max-age`, `crates-db-auto-update`, `crates-indicators`,
`crates-name-completion-limit`, `crates-name-completion-insert-version`,
`crates-name-completion-workspace`,
`crates-name-annotation-width`, and
the faces `crates-up-to-date`, `crates-compatible-update`,
`crates-outdated` and `crates-error`.

## Development

```sh
make deps    # package-lint and the toml grammar, into .cache/
make all     # byte-compile (warnings are errors), checkdoc, package-lint, tests
```

Tests build a database from the tiny dump in `test/fixtures/db-dump`
with the real `curl | tar | sqlite3` pipeline, through a `file://` URL.
To use a grammar you already have, pass `GRAMMAR_DIR=/path/to/dir`.

## License

GPL-3.0-or-later.
