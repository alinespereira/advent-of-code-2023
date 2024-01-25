.PHONY: test

test:
	HSPEC_COLOR=yes cabal test --test-show-details=direct

watch:
	HSPEC_COLOR=yes ghcid --color=always

run:
	cabal run
