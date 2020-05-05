
structure CSVReader = struct

(*!!! should be functorised *)
    structure Map = RedBlackMapFn(struct
                                   type ord_key = string
                                   val compare = String.compare
                                   end)

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

    fun split' (sep : separator, quote : quote_type, esc : escape_type) line =
        let fun isQuote char =
                case quote of
                    NO_QUOTING => false
                  | QUOTE_AUTO => char = #"'" orelse char = #"\""
                  | QUOTE_CHAR qc => char = qc

            fun isSeparator char =
                case sep of
                    SEPARATOR s => char = s orelse
                                   (s = #" " andalso Char.isSpace char)

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
                         if sep = SEPARATOR #" "
                         then (state, false, [], tokens)
                         else (state, false, [], "" :: tokens)
                       | IN_UNQUOTED =>
                         (AFTER_SEPARATOR, false, [],
                          (implode (rev pending)) :: tokens)
                       | IN_QUOTED qc =>
                         (state, false, char :: pending, tokens)
                else if char = #"\\"
                        andalso (esc = ESCAPE_AUTO
                                 orelse esc = ESCAPE_BACKSLASH)
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

    val split =
        split' (SEPARATOR #",", QUOTE_AUTO, ESCAPE_BACKSLASH)
                                            
    fun trim str =
        hd (String.fields (fn x => x = #"\n" orelse x = #"\r") str)

    fun foldlStream f acc stream =
        case TextIO.inputLine stream of
	    SOME line => foldlStream f (f (split line, acc)) stream
          | NONE => acc

    fun foldlFile f acc file =
        let val stream = TextIO.openIn file
            val result = foldlStream f acc stream
        in
            result before TextIO.closeIn stream
        end

    fun mapRow (headers, fields) =
        List.foldl Map.insert' Map.empty (ListPair.zip (headers, fields))

    fun foldlStreamCols f acc (stream, headers) =
        case TextIO.inputLine stream of
	    SOME line => foldlStreamCols f (f (mapRow (headers, split line),
                                               acc))
                                         (stream, headers)
          | NONE => acc

    fun foldlFileCols f acc file =
        let val stream = TextIO.openIn file
            val headers =
                case TextIO.inputLine stream of
                    SOME line => split line
                  | NONE => []
            val result =
                case headers of
                    [] => acc
                  | _ => foldlStreamCols f acc (stream, headers)
        in
            result before TextIO.closeIn stream
        end

    fun loadFile file =
        rev (foldlFile (op::) [] file)

    fun loadFileCols file =
        rev (foldlFileCols (op::) [] file)

    fun loadFileRows file =
        let fun add (row, map) =
                case row of
                    header::rest => Map.insert (map, header, rest)
                  | [] => map
        in
            foldlFile add Map.empty file
        end

    fun loadFileRowsAndCols file =
        let val stream = TextIO.openIn file
            val headers =
                case TextIO.inputLine stream of
                    NONE => []
                  | SOME line =>
                    case split line of
                        rowhead::rest => rest
                      | [] => []
            fun add (row, map) =
                case row of
                    rowhead::rest => Map.insert (map, rowhead,
                                                 mapRow (headers, rest))
                  | [] => map
            val result = foldlStream add Map.empty stream
        in
            result before TextIO.closeIn stream
        end
            
end
