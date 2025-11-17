:- prolog_load_context(directory, Dir),
   directory_file_path(Dir, cid, CidModule),
   use_module(CidModule).
:- use_module(library(apply)).
:- use_module(library(filesex)).
:- use_module(library(lists)).

main :-
    cids_dir(CidsDir),
    directory_files(CidsDir, Entries),
    exclude(is_special, Entries, Files0),
    sort(Files0, Files),
    findall(File-Expected, (
        member(File, Files),
        directory_file_path(CidsDir, File, Path),
        compute_cid(Path, Expected),
        File \= Expected
    ), Mismatches),
    length(Files, Count),
    report(Mismatches, Count).

report([], Count) :-
    format('All ~d CID files match their contents.~n', [Count]),
    halt(0).
report(Mismatches, _) :-
    writeln('Found CID mismatches:'),
    forall(member(File-Expected, Mismatches),
           format('- ~w should be ~w~n', [File, Expected])),
    halt(1).

is_special('.') :- !.
is_special('..').

:- initialization(main, main).
