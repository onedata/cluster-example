REPO	        ?= op-worker

OVERLAY_VARS    ?=

.PHONY: deps test

all: complete_rel

##
## Rebar targets
##

recompile:
	./rebar compile skip_deps=true

##
## If performance is compiled in cluster_worker ten annotations do not work.
## Make sure they are not included in cluster_worker build.
## todo: find better solution
##
compile:
	sed -i "s/ \"deps\/ctool\/annotations\/performance\.erl\"/%%\"deps\/ctool\/annotations\/performance\.erl\"/" deps/cluster_worker/rebar.config
	rm deps/cluster_worker/ebin/performance.beam || true
	./rebar compile
	sed -i "s/%%\"deps\/ctool\/annotations\/performance\.erl\"/ \"deps\/ctool\/annotations\/performance\.erl\"/" deps/cluster_worker/rebar.config

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
## Rename all tests in order to remove duplicated names (add _(++i) suffix to each test)
	@for tout in `find test -name "TEST-*.xml"`; do awk '/testcase/{gsub("_[0-9]+\"", "_" ++i "\"")}1' $$tout > $$tout.tmp; mv $$tout.tmp $$tout; done