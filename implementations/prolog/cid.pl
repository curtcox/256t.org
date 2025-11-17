:- module(cid, [compute_cid/2, cids_dir/1]).

:- use_module(library(apply)).
:- use_module(library(base64)).
:- use_module(library(crypto)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(readutil)).

cids_dir(CidsDir) :-
    prolog_load_context(directory, Dir),
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
    format(atom(Hex), '~16r', [Length]),
    atom_codes(Hex, HexCodes),
    length(HexCodes, HexLen),
    Pad is 12 - HexLen,
    pad_codes(Pad, 0'0, PadCodes),
    append(PadCodes, HexCodes, Padded),
    hex_codes_bytes(Padded, Bytes),
    base64url_bytes(Bytes, Encoded).

content_suffix(Length, Bytes, Suffix) :-
    (   Length =< 64
    ->  base64url_bytes(Bytes, Suffix)
    ;   crypto_data_hash(Bytes, HashBytes, [algorithm(sha512), encoding(octet)]),
        base64url_bytes(HashBytes, Suffix)
    ).

pad_codes(Pad, _, []) :-
    Pad =< 0,
    !.
pad_codes(Pad, Code, [Code|Rest]) :-
    Next is Pad - 1,
    pad_codes(Next, Code, Rest).

hex_codes_bytes(Codes, Bytes) :-
    phrase(hex_bytes(Bytes), Codes).

hex_bytes([]) -->
    [].
hex_bytes([Byte|Rest]) -->
    hex_value(High),
    hex_value(Low),
    {Byte is (High << 4) + Low},
    hex_bytes(Rest).

hex_value(Value) -->
    [Code],
    { code_type(Code, xdigit(Value)) }.

base64url_bytes(Bytes, UrlSafe) :-
    % Encode octets with the portable base64_encoded/3 predicate, then
    % translate to the URL-safe alphabet and strip padding/newlines.
    base64_encoded(Bytes, Base64, [encoding(octet)]),
    atom_codes(Base64, Codes),
    maplist(base64url_char, Codes, UrlCodes0),
    exclude(is_padding, UrlCodes0, UrlCodes),
    atom_codes(UrlSafe, UrlCodes).

base64url_char(0'+, 0'-) :- !.
base64url_char(0'/, 0'_) :- !.
base64url_char(Code, Code).

is_padding(0'=).
is_padding(10).    % newline safety
is_padding(13).    % carriage return safety
