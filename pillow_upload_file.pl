% handle file upload by PiLLoW
%
% call `cio:set_stdin_mode(binary)' before anything
:- module(pillow_upload_file, [upload_file/4,
                               upload_file/2,
                               get_filename/2]).
:- use_module([ library(system),
                library(file_systems),
                library(lists) 
              ]).

:- mode upload_file(+, +, +, -),
        upload_file(+, +),
        get_filename(+, -).

% gen_new_filename(+BaseDirectory, +Prefix, +Suffix, -NewFileName)
% BaseDirectory, Prefix, Suffix are lists of codes
% the NewFileName is an atom 
gen_new_filename(BaseDirectory, Prefix, Suffix, NewFileName) :-
        now(X), number_codes(X, XC),
        append([BaseDirectory, "\\", Prefix, XC, Suffix], L),
        atom_codes(TempNewFileName, L),
        (  file_exists(TempNewFileName)
        -> gen_new_filename(BaseDirectory, Prefix, Suffix, NewFileName)
        ;  NewFileName = TempNewFileName ).

% extract the extension from a path. fail if there hasn't got any extensions
% extract_file_ext(+Path, -Ext)
%
%
% Example:
%
% ?- pillow_upload_file:extract_file_ext('c:/w/pillow1/src/pillow_upload_file.pl', Ext), atom_codes(A, Ext).
% A = '.pl',
% Ext = [46,112,108] ? ;
% no
extract_file_ext(Path, Ext) :-
        atom_codes(Path, PathCodes),
        file_get_ext__full(PathCodes, Ext, Got),
        Got.
file_get_ext__full([], [], false).
file_get_ext__full([C|Rest], Ext, Got) :-
        file_get_ext__full(Rest, Ext1, Got1),
        (  Got1
        -> 
           Ext = Ext1, 
           Got = Got1
        ;  
           ( \+ memberchk(C, "\\/:")
           ->
               Ext = [C|Ext1],
               (  char_code('.', C)
               -> Got = true
               ;  Got = Got1 ) 
           ;   fail ) ).

% save_data(+Data, +Path)
save_data(Data, Path) :-
        open(Path, write, Stream, [type(binary)]),
        save_data__save_lines_list(Data, Stream),
        close(Stream).
save_data__save_bytes([], _).
save_data__save_bytes([C|Rest], Stream) :-
        put_byte(Stream, C),
        save_data__save_bytes(Rest, Stream).
save_data__save_lines_list([], _).
save_data__save_lines_list([L|Rest], Stream) :- 
        save_data__save_bytes(L, Stream),
        save_data__save_lines_list(Rest, Stream).


% upload_file(+FileValue, +BaseDirectory, +Prefix, -NewFileName) 
% BaseDirectory, Prefix, NewFileName are atoms          
upload_file(file(OrigName, Content), BaseDirectory, Prefix, NewFileName) :-
        atom_codes(BaseDirectory, BaseDirectory1),
        atom_codes(Prefix, Prefix1),
        ( extract_file_ext(OrigName, Ext) 
        ->
          gen_new_filename(BaseDirectory1, Prefix1, Ext, NewFileName)
        ;
          gen_new_filename(BaseDirectory1, Prefix1, "", NewFileName) ),
        save_data(Content, NewFileName).
        

% upload_file(+FileValue, +Path) 
% Path is an atom
%
% save FileValue to file `Path'
upload_file(file(_, Content), Path) :-
        save_data(Content, Path).

% get_filename(+FileValue, -FileName).
% FileName is an atom
%
% retrieve the FileName from FileValue
get_filename(file(FileName,_), FileName).