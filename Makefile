run:
	stack build && stack exec peterbot-haskell -- +RTS -N

build:
	stack install --local-bin-path ./out