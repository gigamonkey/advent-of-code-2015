W =

hlint := /Users/peter/Library/Haskell/bin/hlint

source = $(wildcard *.hs)
binaries = $(basename $(source))

all: $(binaries)

test: tic-tac-toe
	./tic-tac-toe

lint:
	$(hlint) $(source)

clean: tidy
	rm -f $(binaries)
	rm -f *.hi
	rm -f *.o

tidy:
	rm -f *~

%: %.hs
	stack ghc -- -O2 -W$(W) $<
