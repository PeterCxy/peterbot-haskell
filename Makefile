run:
	stack build && stack exec peterbot-haskell -- +RTS -N32

build:
	stack install --local-bin-path ./out