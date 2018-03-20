PROJECT = blackjack
PROJECT_DESCRIPTION = Cowboy Websocket example
PROJECT_VERSION = 1
ERLC_OPTS = -W0

DIALYZER_OPTS = -Werror_handling -Wunmatched_returns

# Dependencies.

dep_cowboy = git https://github.com/ninenines/cowboy.git master

dep_jiffy = git https://github.com/davisp/jiffy.git master

include erlang.mk
