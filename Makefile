PROJECT = gwen

# Dependencies
DEPS = lager sync riakc
dep_lager     = https://github.com/basho/lager.git 2.0.3
dep_sync      = https://github.com/rustyio/sync.git master
# dep_poolboy   = https://github.com/devinus/poolboy.git 1.2.1
dep_riakc     = https://github.com/basho/riak-erlang-client.git master

include erlang.mk

ERLC_OPTS += +'{parse_transform, lager_transform}'

run: all
	erl -pa deps/*/ebin ebin -sname gwen@localhost -s gwen start
