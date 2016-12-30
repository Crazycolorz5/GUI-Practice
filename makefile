profile:
	stack build --executable-profiling --ghc-options="-O2 -fprof-auto -rtsopts"
build:
	stack build
exec:
	stack exec GUI-Practice
