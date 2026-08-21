# Makefile for gsh — a Go scratch project for goroutine/channel plumbing.
#
# Everything here is concurrent, so `test` runs with the race detector by default.
# Pass arguments to the program with ARGS, e.g.  make run ARGS="alice bob"

BINARY := gsh
BINDIR := bin
ARGS   := one two three four

GO := go

.DEFAULT_GOAL := build

.PHONY: build test run clean install fmt vet help

# Compile the program into $(BINDIR), keeping the module root free of binaries.
build:
	$(GO) build -o $(BINDIR)/$(BINARY) .

# Race detector on by default: every primitive in this project is goroutine-based.
test:
	$(GO) test -race -v ./...

# Build then execute. Uses the built binary rather than `go run` so what we run
# is exactly what `build` produced.
run: build
	./$(BINDIR)/$(BINARY) $(ARGS)

# Drop the binaries and the test cache, so the next `make test` really re-runs
# the concurrent tests instead of replaying a cached PASS. Also removes a
# root-level $(BINARY), which a bare `go build ./...` leaves in the cwd.
clean:
	rm -rf $(BINDIR) ./$(BINARY)
	$(GO) clean -testcache

# Placeholder for future install work. Deliberately does nothing yet: it will
# become `$(GO) install ./...`, dropping the binary into $$(go env GOBIN).
install:
	@echo "install: placeholder — will run '$(GO) install ./...' into $$($(GO) env GOBIN)"

fmt:
	gofmt -w .

vet:
	$(GO) vet ./...

help:
	@echo "targets:"
	@echo "  build    compile into $(BINDIR)/$(BINARY)  (default)"
	@echo "  test     go test"
	@echo "  run      build, then run; pass args with ARGS=\"alice bob\""
	@echo "  clean    remove $(BINDIR) and the test cache"
	@echo "  install  placeholder for future go install"
	@echo "  fmt      gofmt -w ."
	@echo "  vet      go vet ./..."
