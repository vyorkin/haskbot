
dev:
	stack build --test --no-run-tests
	ghcid --command="cabal repl" | source-highlight -s haskell -f esc
repl:
	cabal repl
build:
	cabal build
clean:
	cabal clean
check:
	cabal check
tags:
	rm -f tags TAGS ctags codex.tags
	# see: https://github.com/aloiscochard/codex/issues/86
	mv .stack-work .stack-work-bak
	codex update --force
	mv .stack-work-bak .stack-work
	haskdogs --hasktags-args "-b"
prof:
	cabal configure --enable-profiling
noprof:
	cabal configure --disable-profiling

.PHONY: dev repl build clean check tags prof noprof
