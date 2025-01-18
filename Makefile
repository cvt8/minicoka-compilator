
all: kokac.exe
	dune exec ./kokac.exe test0.koka

tests: kokac.exe
	for f in tests/*.logo; do dune exec ./kokac.exe $$f; done

kokac.exe:
	dune build kokac.exe

explain:
	menhir --base /tmp/parser --dump --explain parser.mly
	cat /tmp/parser.conflicts

clean:
	dune clean

.PHONY: all clean explain kokac.exe


