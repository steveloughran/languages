# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

`gsh` is a Go scratch project for exploring goroutine/channel plumbing. It lives inside the larger
`languages` polyglot playground repo (git root is `/Volumes/ssd1t/Play/languages`; each language
subdirectory is an independent project). Everything here is package `main` at the module root —
`main.go` holds the primitives, `gsh_test.go` exercises them.

Module: `gsh`, Go 1.26. Sole direct dependency is `stretchr/testify` (assertions).

## Build & Test

```bash
go build ./...
go vet ./...
gofmt -l .              # list unformatted files; -w to rewrite

go test ./...
go test -race ./...     # use this by default — everything here is concurrent
go test -run TestFanOut -v ./...   # single test
```

There is no linter configured beyond `go vet` (no golangci-lint / staticcheck installed).

## Architecture

Three composable channel primitives in `main.go`, each of which starts a goroutine and returns the
channels the caller drives it through. The contracts matter more than the code:

- **`stdout() chan string`** — sink. Ranges over the returned channel and prints each message. Its
  goroutine is never shut down explicitly.
- **`launch(name, printer) (chan string, chan bool)`** — named worker. Ranges over `input`,
  forwarding `name + ": " + s` to `printer`. When `input` is closed it emits `"name: Stopped!"`,
  then **sends `true` on `stopper` and closes it**. The send is unbuffered, so the caller must
  receive from `stopper` or the goroutine leaks.
- **`fanOut(left, right) chan string`** — duplicator. Forwards each message to `left` then `right`
  (sequentially, so a blocked receiver on `left` stalls the whole path). Closing the returned
  channel closes **both** `left` and `right`.

Composition (see `TestMulti`): `stdout()` feeds two `launch` workers, whose input channels are
wired into `fanOut`. Closing the fan-out channel cascades: both worker inputs close, both workers
emit their stop message, both `stopper` channels fire.

All channels are unbuffered. Ownership rule throughout: whoever creates a channel's *producer* side
closes it, and closing propagates downstream — never close a channel a `launch`/`fanOut` goroutine
owns the write side of.

## Conventions

From the repo-wide `.github/copilot-instructions.md`: write clear, commented code; prefer simplicity
over clever constructs; keep tests. Doc comments here use `/* ... */` blocks describing the channel
contract (what is returned, what closes what) — follow that pattern when adding primitives.
