
signature CSV_SPLITTER = sig

    (** Field separator *)
    datatype separator = SEPARATOR of char

    (** Type of field quote character to expect. QUOTE_CHAR specifies
        a single specific character, while QUOTE_AUTO permits either
        single or double quote. *)
    datatype quote_type = NO_QUOTING
                        | QUOTE_AUTO
                        | QUOTE_CHAR of char

    (** Type of escape to support within a field. ESCAPE_BACKSLASH
        allows quote characters to be escaped with a preceding
        backslash, ESCAPE_DOUBLING allows a doubled quote to represent
        a single quote character within a quoted string, and
        ESCAPE_AUTO allows either. *)
    datatype escape_type = ESCAPE_AUTO
                         | ESCAPE_BACKSLASH
                         | ESCAPE_DOUBLING

    (** Parameters to use when splitting. *)
    type params = {
        separator : separator,
        quote_type : quote_type,
        escape_type : escape_type
    }

    (** "Standard" parameters, consisting of comma separator and auto
        quote and escape parameters. *)
    val defaultParams : params

    (** Split the given string using the given parameters. *)
    val split : params -> string -> string list

end
                             
