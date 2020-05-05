
signature CSV_SPLITTER = sig

    datatype separator = SEPARATOR of char
                                 
    datatype quote_type = NO_QUOTING
                        | QUOTE_AUTO
                        | QUOTE_CHAR of char
                                 
    datatype escape_type = ESCAPE_AUTO
                         | ESCAPE_BACKSLASH
                         | ESCAPE_DOUBLING

    type params = {
        separator : separator,
        quote_type : quote_type,
        escape_type : escape_type
    }

    val split : params -> string -> string list

end
                             
