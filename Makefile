all: test

test: test-state test-reader test-writer

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
  
test-reader:
	mkdir -p js/test
	
	psc src\Control\Monad\Trans.purs \
	    src\Control\Monad\Identity.purs \
	    src\Control\Monad\Reader.purs \
	    src\Control\Monad\Reader\Class.purs \
	    src\Control\Monad\Reader\Trans.purs \
	    examples\Reader.purs \
	  -o js/test/reader.js \
	  --main \
	  --module Main \
	  --tco --magic-do
	
	node js/test/reader.js
  
test-writer:
	mkdir -p js/test
	
	psc src\Control\Monad\Trans.purs \
	    src\Control\Monad\Identity.purs \
	    src\Control\Monad\Writer.purs \
	    src\Control\Monad\Writer\Class.purs \
	    src\Control\Monad\Writer\Trans.purs \
	    examples\Writer.purs \
	  -o js/test/writer.js \
	  --main \
	  --module Main \
	  --tco --magic-do
	
	node js/test/writer.js