-define(debug,yes).

-ifdef(debug).
-define(print(S), io:format("~s: ~s~n",[?MODULE_STRING,S])).
-define(printf(S,A), io:format("~s: "++S ++"~n",[?MODULE_STRING|A])).

-else.

-define(print(S), void).
-define(printf(S,A), void).
-endif.


