
structure CSVSplitter = struct

    datatype separator = SEPARATOR of char
                                 
    datatype quote_type = NO_QUOTING
                        | QUOTE_AUTO
                        | QUOTE_CHAR of char
                                 
    datatype escape_type = ESCAPE_AUTO
                         | ESCAPE_BACKSLASH
                         | ESCAPE_DOUBLING
                              
    datatype split_state = AFTER_SEPARATOR
                         | IN_UNQUOTED
                         | IN_QUOTED of char

    type params = {
        separator : separator,
        quote_type : quote_type,
        escape_type : escape_type
    }
                                            
    fun split (params : params) line =
        let fun isQuote char =
                case (#quote_type params) of
                    NO_QUOTING => false
                  | QUOTE_AUTO => char = #"'" orelse char = #"\""
                  | QUOTE_CHAR qc => char = qc

            fun isSeparator char =
                case (#separator params) of
                    SEPARATOR s => char = s orelse
                                   (s = #" " andalso Char.isSpace char)

            fun isBackslashEscape char =
                case (#escape_type params, char) of
                    (ESCAPE_AUTO, #"\\") => true
                  | (ESCAPE_BACKSLASH, #"\\") => true
                  | _ => false

            (*!!! todo: ESCAPE_DOUBLING *)
            fun consume (char, (state, escaping, pending, tokens)) =
                if escaping
                then (state, false, char :: pending, tokens)
                else if isQuote char
                then case state of
                         AFTER_SEPARATOR =>
                         (IN_QUOTED char, false, [], tokens)
                       | IN_UNQUOTED =>
                         (state, false, char :: pending, tokens)
                       | IN_QUOTED qc =>
                         if char <> qc
                         then (state, false, char :: pending, tokens)
                         else (IN_UNQUOTED, false, pending, tokens)
                else if isSeparator char
                then case state of
                         AFTER_SEPARATOR =>
                         if (#separator params) = SEPARATOR #" "
                         then (state, false, [], tokens)
                         else (state, false, [], "" :: tokens)
                       | IN_UNQUOTED =>
                         (AFTER_SEPARATOR, false, [],
                          (implode (rev pending)) :: tokens)
                       | IN_QUOTED qc =>
                         (state, false, char :: pending, tokens)
                else if isBackslashEscape char
                then case state of
                         AFTER_SEPARATOR =>
                         (IN_UNQUOTED, true, [], tokens)
                       | _ =>
                         (state, true, pending, tokens)
                else case state of
                         AFTER_SEPARATOR =>
                         (IN_UNQUOTED, false, [char], tokens)
                       | _ =>
                         (state, false, char :: pending, tokens)

            val (state, escaping, pending, tokens) = 
                foldl consume (AFTER_SEPARATOR, false, [], []) (explode line)

            val (state, escaping, pending, tokens) =
                if escaping
                then (state, false, #"\\" :: pending, tokens)
                else (state, false, pending, tokens)
        in
            rev (case (state, pending, tokens) of
                     (AFTER_SEPARATOR, pending, tokens) =>
                     (implode (rev pending)) :: tokens
                   | (IN_UNQUOTED, pending, tokens) =>
                     (implode (rev pending)) :: tokens
                   | (IN_QUOTED qc, pending, tokens) => (* not quoted after all *)
                     (implode (qc :: (rev pending))) :: tokens)
        end
end
                            