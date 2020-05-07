
structure CSVSplitter : CSV_SPLITTER = struct

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
                              
    datatype split_state = AT_START
                         | AFTER_SEPARATOR
                         | IN_UNQUOTED
                         | IN_QUOTED of char

    datatype escape_state = NOT_ESCAPING
                          | BACKSLASH_ESCAPING
                          | DOUBLE_ESCAPING of char
                                            
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

            val haveDoublingEscapes =
                case #escape_type params of
                    ESCAPE_AUTO => true
                  | ESCAPE_DOUBLING => true
                  | _ => false
                             
            fun consume (char, (state, BACKSLASH_ESCAPING, pending, tokens)) =
                (state, NOT_ESCAPING, char :: pending, tokens)
              | consume (char, (state, DOUBLE_ESCAPING eq, pending, tokens)) =
                if char = eq
                then (state, NOT_ESCAPING, char :: pending, tokens)
                else consume (char, (IN_UNQUOTED, NOT_ESCAPING, pending, tokens))
              | consume (char, (state, NOT_ESCAPING, pending, tokens)) =
                if isQuote char
                then case state of
                         IN_UNQUOTED =>
                         (state, NOT_ESCAPING, char :: pending, tokens)
                       | IN_QUOTED qc =>
                         if char = qc
                         then if haveDoublingEscapes
                              then (state, DOUBLE_ESCAPING qc, pending, tokens)
                              else (IN_UNQUOTED, NOT_ESCAPING, pending, tokens)
                         else (state, NOT_ESCAPING, char :: pending, tokens)
                       | _ => 
                         (IN_QUOTED char, NOT_ESCAPING, [], tokens)
                else if isSeparator char
                then case state of
                         AT_START =>
                         (AFTER_SEPARATOR, NOT_ESCAPING, [], "" :: tokens)
                       | AFTER_SEPARATOR =>
                         if (#separator params) = SEPARATOR #" "
                         then (state, NOT_ESCAPING, [], tokens)
                         else (state, NOT_ESCAPING, [], "" :: tokens)
                       | IN_UNQUOTED =>
                         (AFTER_SEPARATOR, NOT_ESCAPING, [],
                          (implode (rev pending)) :: tokens)
                       | IN_QUOTED qc =>
                         (state, NOT_ESCAPING, char :: pending, tokens)
                else if isBackslashEscape char
                then case state of
                         AT_START =>
                         (IN_UNQUOTED, BACKSLASH_ESCAPING, [], tokens)
                       | AFTER_SEPARATOR =>
                         (IN_UNQUOTED, BACKSLASH_ESCAPING, [], tokens)
                       | _ =>
                         (state, BACKSLASH_ESCAPING, pending, tokens)
                else case state of
                         AT_START => 
                         (IN_UNQUOTED, NOT_ESCAPING, [char], tokens)
                       | AFTER_SEPARATOR =>
                         (IN_UNQUOTED, NOT_ESCAPING, [char], tokens)
                       | _ =>
                         (state, NOT_ESCAPING, char :: pending, tokens)

            val (state, escaping, pending, tokens) = 
                foldl consume (AT_START, NOT_ESCAPING, [], []) (explode line)

            val (state, escaping, pending, tokens) =
                case escaping of
                    NOT_ESCAPING =>
                    (state, escaping, pending, tokens)
                  | BACKSLASH_ESCAPING => 
                    (state, NOT_ESCAPING, #"\\" :: pending, tokens)
                  | DOUBLE_ESCAPING eq => (* just a normal close-quote at EOL *)
                    (IN_UNQUOTED, NOT_ESCAPING, pending, tokens)
        in
            rev (case (state, pending, tokens) of
                     (AT_START, pending, tokens) =>
                     tokens
                   | (AFTER_SEPARATOR, pending, tokens) =>
                     (implode (rev pending)) :: tokens
                   | (IN_UNQUOTED, pending, tokens) =>
                     (implode (rev pending)) :: tokens
                   | (IN_QUOTED qc, pending, tokens) => (* not quoted after all *)
                     (implode (qc :: (rev pending))) :: tokens)
        end
end
                            
