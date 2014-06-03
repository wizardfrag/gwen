PROJECT = gwen

# Dependencies
DEPS = poolboy epgsql lager sync
dep_epgsql  = https://github.com/opscode/epgsql.git 1.4
dep_lager   = https://github.com/basho/lager.git 2.0.3
dep_sync    = https://github.com/rustyio/sync.git master
dep_poolboy = https://github.com/devinus/poolboy.git 1.2.1

include erlang.mk

ERLC_OPTS += +'{parse_transform, lager_transform}'