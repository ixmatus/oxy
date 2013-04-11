all: configure compile

compile:
	cabal build

configure:
	cabal configure

clean:
	cabal clean
