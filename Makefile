run:
	stack build && stack exec peterbot-haskell

build:
	stack install --local-bin-path ./out