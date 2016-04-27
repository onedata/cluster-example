GIT_URL := $(shell git config --get remote.origin.url | sed -e 's/\(\/[^/]*\)$$//g')
GIT_URL := $(shell if [ "${GIT_URL}" = "file:/" ]; then echo 'ssh://git@git.plgrid.pl:7999/vfs'; else echo ${GIT_URL}; fi)
ONEDATA_GIT_URL := $(shell if [ "${ONEDATA_GIT_URL}" = "" ]; then echo ${GIT_URL}; else echo ${ONEDATA_GIT_URL}; fi)
export ONEDATA_GIT_URL

OVERLAY_VARS    ?=

.PHONY: deps test

all: complete_rel

##
## Rebar targets
##

recompile:
	./rebar compile skip_deps=true

compile:
	./rebar compile

deps:
	./rebar get-deps

##
## Reltool configs introduce dependency on deps directories (which do not exist)
## Also a release is not nescesary for us.
## We prevent reltool from creating a release.
## todo: find better solution
##
generate: deps compile
	sed -i "s/{sub_dirs, \[\"rel\"\]}\./{sub_dirs, \[\]}\./" deps/cluster_worker/rebar.config
	./rebar generate $(OVERLAY_VARS)
	sed -i "s/{sub_dirs, \[\]}\./{sub_dirs, \[\"rel\"\]}\./" deps/cluster_worker/rebar.config

clean: relclean
	./rebar clean

distclean:
	./rebar delete-deps

##
## Release targets
##

rel: generate

complete_rel: generate cm_rel

cm_rel:
	rm -rf cluster_manager || true
	ln -sf deps/cluster_worker/cluster_manager/
	make -C cluster_manager/ rel

relclean:
	rm -rf rel/test_cluster
	rm -rf rel/example
	rm -rf cluster_manager/rel/cluster_manager

##
## Testing targets
##

eunit:
	./rebar eunit skip_deps=true suites=${SUITES}