all: test

test: test-error test-state test-reader test-writer

test-error:
	psc src\Control\Monad\Trans.purs \
	    src\Control\Monad\Identity.purs \
	    src\Control\Monad\Error.purs \
	    src\Control\Monad\Error\Class.purs \
	    src\Control\Monad\Error\Trans.purs \
	    src\Control\Monad\Maybe\Trans.purs \
	    src\Control\Monad\Reader\Trans.purs \
	    src\Control\Monad\Writer\Trans.purs \
	    src\Control\Monad\State\Trans.purs \
	  -o js/test/error.js \
	  --tco --magic-do
	
	node js/test/error.js

test-state:
	psc src\Control\Monad\Trans.purs \
	    src\Control\Monad\Identity.purs \
	    src\Control\Monad\State.purs \
	    src\Control\Monad\State\Class.purs \
	    src\Control\Monad\State\Trans.purs \
	    src\Control\Monad\Error.purs \
	    src\Control\Monad\Error\Trans.purs \
	    src\Control\Monad\Reader\Trans.purs \
	    src\Control\Monad\Writer\Trans.purs \
	    src\Control\Monad\Maybe\Trans.purs \
	    examples\State.purs \
	  -o js/test/state.js \
	  --main \
	  --module Main \
	  --tco --magic-do
	
	node js/test/state.js
  
test-reader:
	psc src\Control\Monad\Trans.purs \
	    src\Control\Monad\Identity.purs \
	    src\Control\Monad\Reader.purs \
	    src\Control\Monad\Reader\Class.purs \
	    src\Control\Monad\Reader\Trans.purs \
	    src\Control\Monad\Error.purs \
	    src\Control\Monad\Error\Trans.purs \
	    src\Control\Monad\Maybe\Trans.purs \
	    src\Control\Monad\Writer\Trans.purs \
	    src\Control\Monad\State\Trans.purs \
	    examples\Reader.purs \
	  -o js/test/reader.js \
	  --main \
	  --module Main \
	  --tco --magic-do
	
	node js/test/reader.js
  
test-writer:
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
