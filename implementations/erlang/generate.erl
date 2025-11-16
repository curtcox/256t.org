-module(generate).
-export([run/0, main/1]).

main(_Args) ->
    run().

run() ->
    cid:write_example_cids(),
    halt(0).
