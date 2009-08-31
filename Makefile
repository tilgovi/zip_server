
build = \
    couch_file.beam \
    zip_server.beam \
	zip_file.beam \
    file_test.beam

all: $(build)
	./test.es

%.beam: %.erl
	erlc $<
