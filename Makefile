
build = \
    ebin/couch_file.beam \
    ebin/zip_server.beam \
	ebin/zip_file.beam \
    ebin/file_test.beam

all: $(build)
	./test.es

ebin/%.beam: src/%.erl
	@if test ! -d ebin/ ; then mkdir ebin ; fi
	erlc -o ebin/ $<
