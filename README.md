chat_srv
=====
Chat server,

An OTP application

Build:  $ rebar3 compile
Test:   $ rebar3 eunit
Run:    $ rebar3 shell --name "some_erlang_node_name"
	    and then in it: 'application:start(chat_srv)' 
  or:   $ erl -pa _build/default/lib/chat_srv/ebin/ -name "some_erlang_node_name" -eval 'application:start(chat_srv)'

