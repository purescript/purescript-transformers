all: test

test: test-state

test-state:
	mkdir -p js/test
	
	psc src\Control\Monad\Trans.purs \
	    src\Control\Monad\Identity.purs \
	    src\Control\Monad\State.purs \
	    src\Control\Monad\State\Class.purs \
	    src\Control\Monad\State\Trans.purs \
	    examples\State.purs \
	  -o js/test/state.js \
	  --main \
	  --module Main \
	  --tco --magic-do
	
	node js/test/state.js
