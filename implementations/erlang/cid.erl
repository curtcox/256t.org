-module(cid).
-export([base_dir/0, examples_dir/0, cids_dir/0, encode_length/1, to_base64url/1, compute/1, write_example_cids/0]).

base_dir() ->
    filename:dirname(filename:dirname(filename:dirname(code:which(?MODULE)))).

examples_dir() ->
    filename:join(base_dir(), "examples").

cids_dir() ->
    filename:join(base_dir(), "cids").

encode_length(Length) when is_integer(Length), Length >= 0 ->
    LengthBin = <<Length:48/unsigned-big>>,
    to_base64url(LengthBin).

to_base64url(Bin) when is_binary(Bin) ->
    base64url(base64:encode(Bin));

to_base64url(List) when is_list(List) ->
    to_base64url(list_to_binary(List)).

base64url(Encoded) ->
    binary:replace(
      binary:replace(
        binary:replace(Encoded, <<"+">>, <<"-">>, [global]),
        <<"/">>, <<"_">>, [global]),
      <<"=">>, <<>>, [global]
    ).

compute(Content) when is_binary(Content) ->
    Prefix = encode_length(byte_size(Content)),
    Suffix = case byte_size(Content) =< 64 of
        true -> to_base64url(Content);
        false -> to_base64url(crypto:hash(sha512, Content))
    end,
    <<Prefix/binary, Suffix/binary>>;
compute(Content) when is_list(Content) ->
    compute(list_to_binary(Content)).

write_example_cids() ->
    filelib:ensure_dir(filename:join(cids_dir(), "placeholder")),
    {ok, Entries} = file:list_dir(examples_dir()),
    lists:foreach(fun(Name) ->
        Path = filename:join(examples_dir(), Name),
        case filelib:is_regular(Path) of
            true ->
                {ok, Content} = file:read_file(Path),
                Cid = compute(Content),
                ok = file:write_file(filename:join(cids_dir(), Cid), Content),
                io:format("Wrote ~s from ~s~n", [Cid, Name]);
            false ->
                ok
        end
    end, lists:sort(Entries)).
