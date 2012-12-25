:- use_module(library(pillow)).
:- use_module(library(system)).
:- use_module(library(pillow_upload_file)).
:- use_module(library(cio)).

main :-
    cio:set_stdin_mode(binary),
    get_form_input(Input),
    get_form_value(Input, tag,Name),
    get_form_value(Input, the_file, File),
    upload_file(File, 'c:\\temp', 'temp_upload-', NewFileName),
    output_html([
        cgi_reply,
        start,
        title('Upload Demo'),
        p(['TAG is ', Name]),
        p(['New file name: ', NewFileName]),
        end]).

user:runtime_entry(_) :-
        main.

:- save_program(upload), halt.