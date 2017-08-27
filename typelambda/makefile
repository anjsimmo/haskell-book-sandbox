build: codegen.sh Histogram.m4 testgen.sh Actual/*.m4
	./codegen.sh
	./testgen.sh

run: build
	stack runghc Main.hs

test: runtests.sh build
	./runtests.sh

clean:
	rm Histogram.hs
	rm Actual/*.hs
