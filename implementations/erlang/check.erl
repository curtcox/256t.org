-module(check).
-export([run/0, main/1]).

main(_Args) ->
    run().

run() ->
    {Mismatches, Count} = verify_cids(),
    case Mismatches of
        [] ->
            io:format("All ~p CID files match their contents.~n", [Count]),
            halt(0);
        _ ->
            lists:foreach(
              fun({Actual, Expected}) ->
                  io:format("~s should be ~s~n", [Actual, Expected])
              end,
              lists:reverse(Mismatches)),
            io:format("Found ~p mismatched CID file(s).~n", [length(Mismatches)]),
            halt(1)
    end.

verify_cids() ->
    CidsDir = cid:cids_dir(),
    {ok, Entries} = file:list_dir(CidsDir),
    lists:foldl(
      fun(Name, {Acc, Count}) ->
          Path = filename:join(CidsDir, Name),
          case filelib:is_regular(Path) of
              true ->
                  {ok, Content} = file:read_file(Path),
                  Expected = cid:compute(Content),
                  Actual = list_to_binary(Name),
                  NewAcc = case Expected =:= Actual of
                      true -> Acc;
                      false -> [{Actual, Expected} | Acc]
                  end,
                  {NewAcc, Count + 1};
              false ->
                  {Acc, Count}
          end
      end,
      {[], 0},
      lists:sort(Entries)).
