:- use_module(library(pillow)).
:- use_module(library(pillow_upload_file)).
:- use_module(library(cio)).

main :-
    set_stdin_mode(binary),
    get_form_input(Input),
    get_form_value(Input, the_file, File),
    get_filename(File, OrigFileName),
    atom_concat('c:\\temp\\', OrigFileName, NewPath),
    upload_file(File, NewPath),
    output_html([
        cgi_reply,
        start,
        title('Upload Demo'),
        p(['Saved to: ', NewPath]),
        end]).

user:runtime_entry(_) :-
        main.

:- save_program(upload1), halt.