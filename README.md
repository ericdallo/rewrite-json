# rewrite-json

[![Clojars Project](https://img.shields.io/clojars/v/dev.ericdallo/rewrite-json.svg)](https://clojars.org/dev.ericdallo/rewrite-json)
[![CI](https://github.com/ericdallo/rewrite-json/actions/workflows/ci.yml/badge.svg)](https://github.com/ericdallo/rewrite-json/actions/workflows/ci.yml)

A Clojure library for **format-preserving** JSON and JSONC editing.

Edit JSON files programmatically without changing existing formatting, indentation, whitespace, or comments. Perfect for tools that need to modify configuration files (like `tsconfig.json`, VS Code `settings.json`, `package.json`, etc.) while respecting the original author's style.

## Features

- **Format preservation** — Whitespace, indentation, and newline style are kept exactly as-is
- **JSONC support** — Line comments (`//`), block comments (`/* */`), and trailing commas
- **Zipper API** — Powerful tree navigation and editing (à la [rewrite-clj](https://github.com/clj-commons/rewrite-clj))
- **Path API** — Simple `get-in` / `assoc-in` / `update-in` / `dissoc-in` convenience layer
- **Zero dependencies** — Only requires Clojure itself

## Installation

### deps.edn

```clojure
dev.ericdallo/rewrite-json {:mvn/version "0.1.0"}
```

### Leiningen

```clojure
[dev.ericdallo/rewrite-json "0.1.0"]
```

## Quick Start

### Path-based API (simple)

```clojure
(require '[rewrite-json.core :as rj])

(def json-str "{\n  \"name\": \"my-app\",\n  \"version\": \"1.0.0\"\n}")

;; Read a value
(rj/get-in (rj/parse-string json-str) ["version"])
;; => "1.0.0"

;; Update a value — formatting is preserved!
(-> (rj/parse-string json-str)
    (rj/assoc-in ["version"] "2.0.0")
    rj/to-string)
;; => "{\n  \"name\": \"my-app\",\n  \"version\": \"2.0.0\"\n}"
```

### Zipper API (powerful)

```clojure
(require '[rewrite-json.zip :as z])
(require '[rewrite-json.node :as node])

;; Navigate and edit
(-> (z/of-string "{\"editor.fontSize\": 14, \"editor.tabSize\": 2}")
    (z/get-key "editor.fontSize")
    (z/replace (node/number-node 16))
    z/root-string)
;; => "{\"editor.fontSize\": 16, \"editor.tabSize\": 2}"
```

### JSONC (comments preserved!)

```clojure
(def settings "{\n  // Font settings\n  \"editor.fontSize\": 14\n}")

(-> (rj/parse-string settings)
    (rj/assoc-in ["editor.fontSize"] 16)
    rj/to-string)
;; => "{\n  // Font settings\n  \"editor.fontSize\": 16\n}"
```

## API Reference

### `rewrite-json.core` — Path-based API

| Function | Description |
|---|---|
| `(parse-string s)` | Parse a JSON/JSONC string into a node tree |
| `(to-string root)` | Serialize a node tree back to string |
| `(get-in root path)` | Get the value at a key/index path |
| `(get-in root path not-found)` | Get with a default for missing paths |
| `(assoc-in root path v)` | Set a value at path (creates entry if missing) |
| `(update-in root path f & args)` | Update a value by applying a function |
| `(dissoc-in root path)` | Remove an entry at path |

Paths are vectors of string keys and/or integer indices:
```clojure
["dependencies" "react"]    ;; nested object keys
["items" 0 "name"]          ;; mix of keys and array indices
```

### `rewrite-json.zip` — Zipper API

| Function | Description |
|---|---|
| `(of-string s)` | Parse and create a zipper |
| `(of-node root)` | Create a zipper from an existing node tree |
| `(root-string loc)` | Serialize the whole tree back to string |
| `(down loc)` | Move to first meaningful child |
| `(up loc)` | Move to parent |
| `(right loc)` / `(left loc)` | Move between siblings (skips whitespace) |
| `(find-key loc key-name)` | Find an object entry by key |
| `(get-key loc key-name)` | Navigate to the value of a key |
| `(replace loc node)` | Replace the current node |
| `(edit loc f & args)` | Edit current node with a function |
| `(remove loc)` | Remove current node (handles comma cleanup) |
| `(append-entry loc key val-node)` | Append entry to object |
| `(append-element loc val-node)` | Append element to array |
| `(node loc)` / `(value loc)` | Access current node or its Clojure value |

### `rewrite-json.node` — Node types

| Constructor | Description |
|---|---|
| `(string-node s)` | JSON string node |
| `(number-node n)` | JSON number node |
| `(boolean-node b)` | JSON boolean node |
| `(null-node)` | JSON null node |
| `(object-node children)` | JSON object |
| `(array-node children)` | JSON array |
| `(value->node x)` | Convert any Clojure value to a node tree |

## How It Works

`rewrite-json` parses JSON/JSONC into a tree of typed nodes where **every character** from the original source is captured — including whitespace, comments, commas, and colons. When you edit a value and serialize back, only the edited value changes; everything else is reproduced verbatim from the original.

The zipper (built on `clojure.zip`) provides navigation that automatically skips over whitespace and comment nodes, so you work with the semantic structure while the formatting rides along.

## License

MIT License — Copyright (c) 2025 Eric Dallo
