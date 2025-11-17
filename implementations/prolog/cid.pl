:- module(cid, [compute_cid/2, cids_dir/1]).

:- use_module(library(apply)).
:- use_module(library(base64)).
:- use_module(library(crypto)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(readutil)).

:- dynamic cached_cids_dir/1.

cids_dir(CidsDir) :-
    cached_cids_dir(CidsDir),
    !.
cids_dir(CidsDir) :-
    locate_cids_dir(CidsDir0),
    asserta(cached_cids_dir(CidsDir0)),
    CidsDir = CidsDir0.

locate_cids_dir(CidsDir) :-
    (   source_file(cids_dir(_), Source)
    ->  file_directory_name(Source, Dir)
    ;   prolog_load_context(directory, Dir)
    ),
    directory_file_path(Dir, '..', ImplDir),
    directory_file_path(ImplDir, '..', Base0),
    absolute_file_name(Base0, Base, [file_type(directory)]),
    directory_file_path(Base, cids, CidsDir).

compute_cid(File, CID) :-
    read_file_to_codes(File, Bytes, [type(binary)]),
    length(Bytes, Length),
    encode_length(Length, Prefix),
    content_suffix(Length, Bytes, Suffix),
    atomic_list_concat([Prefix, Suffix], CID).

encode_length(Length, Encoded) :-
    length_bytes_48(Length, Bytes),
    base64url_bytes(Bytes, Encoded).

length_bytes_48(Length, Bytes) :-
    Int is Length /\ 0xFFFFFFFFFFFF,
    bytes_from_int(6, Int, Bytes).

bytes_from_int(0, _, []) :- !.
bytes_from_int(N, Int, [Byte|Rest]) :-
    Shift is (N - 1) * 8,
    Byte is (Int >> Shift) /\ 0xFF,
    NextN is N - 1,
    bytes_from_int(NextN, Int, Rest).

content_suffix(Length, Bytes, Suffix) :-
    (   Length =< 64
    ->  base64url_bytes(Bytes, Suffix)
    ;   hash_octets(Bytes, HashBytes),
        base64url_bytes(HashBytes, Suffix)
    ).

hash_octets(Bytes, Octets) :-
    crypto_data_hash(Bytes, Hash, [algorithm(sha512)]),
    normalize_hash(Hash, Octets).

normalize_hash(Hash, Octets) :-
    (   is_list(Hash)
    ->  normalize_hash_list(Hash, Octets)
    ;   atom(Hash)
    ->  atom_codes(Hash, Codes),
        normalize_hash_list(Codes, Octets)
    ;   string(Hash)
    ->  string_codes(Hash, Codes),
        normalize_hash_list(Codes, Octets)
    ).

normalize_hash_list([First,Second|Rest], Octets) :-
    is_hex_digit(First),
    is_hex_digit(Second),
    length(Rest, TailLen),
    TailLen mod 2 =:= 0,
    hex_pairs_to_bytes([First,Second|Rest], Octets),
    !.
normalize_hash_list(List, List).

hex_pairs_to_bytes([], []).
hex_pairs_to_bytes([Hi,Lo|RestCodes], [Byte|RestBytes]) :-
    hex_value(Hi, HighVal),
    hex_value(Lo, LowVal),
    Byte is (HighVal << 4) + LowVal,
    hex_pairs_to_bytes(RestCodes, RestBytes).

hex_value(Code, Value) :-
    Code >= 0'0, Code =< 0'9,
    Value is Code - 0'0.
hex_value(Code, Value) :-
    Code >= 0'a, Code =< 0'f,
    Value is 10 + Code - 0'a.
hex_value(Code, Value) :-
    Code >= 0'A, Code =< 0'F,
    Value is 10 + Code - 0'A.

is_hex_digit(Code) :-
    (Code >= 0'0, Code =< 0'9
    ; Code >= 0'a, Code =< 0'f
    ; Code >= 0'A, Code =< 0'F).

base64url_bytes(Bytes, UrlSafe) :-
    % Encode octets, translate to the URL-safe alphabet, and strip
    % padding/newlines. Newer SWI releases ship base64_encoded/3, while
    % older ones only provide base64/2, so we select the available
    % predicate at runtime for compatibility.
    base64_octets(Bytes, Base64),
    atom_codes(Base64, Codes),
    maplist(base64url_char, Codes, UrlCodes0),
    exclude(is_padding, UrlCodes0, UrlCodes),
    atom_codes(UrlSafe, UrlCodes).

:- if(current_predicate(base64_encoded/3)).
base64_octets(Bytes, Base64) :-
    base64_encoded(Bytes, Base64, [encoding(octet)]).
:- else.
base64_octets(Bytes, Base64) :-
    string_codes(String, Bytes),
    base64(String, Base64).
:- endif.

base64url_char(0'+, 0'-) :- !.
base64url_char(0'/, 0'_) :- !.
base64url_char(Code, Code).

is_padding(0'=).
is_padding(10).    % newline safety
is_padding(13).    % carriage return safety
