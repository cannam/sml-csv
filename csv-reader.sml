
structure CSVReader : CSV_READER = struct

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

    fun split (sep : separator, quote : quote_type, esc : escape_type) line =
        let fun isQuote char =
                case quote of
                    NO_QUOTING => false
                  | QUOTE_AUTO => char = #"'" orelse char = #"\""
                  | QUOTE_CHAR qc => char = qc

            fun isSeparator char =
                char = sep orelse
                (sep = #" " andalso Char.isSpace char)

            fun consume (char, (state, pending, tokens)) =
                if isQuote char
                then case state of
                         AFTER_SEPARATOR => (IN_QUOTED char, [], tokens)
                       | IN_UNQUOTED => (state, char :: pending, tokens)
                       | IN_QUOTED qc => if char <> qc
                                         then (state, char :: pending, tokens)
                                         else (IN_UNQUOTED, pending, tokens)
                else if isSeparator char
                then case state of
                         AFTER_SEPARATOR => if sep = #" "
                                            then (state, [], tokens)
                                            else (state, [], "" :: tokens)
                       | IN_UNQUOTED => (AFTER_SEPARATOR, [],
                                         (implode (rev pending)) :: tokens)
                       | IN_QUOTED qc => (state, char :: pending, tokens)
                else if char = #"\\"
                                   
            
        in
            rev (foldl consume (AFTER_SEPARATOR, [], []) (explode line))
        end
                                            
    fun trim str =
        hd (String.fields (fn x => x = #"\n" orelse x = #"\r") str)

    fun split line =
        String.fields (fn c => c = #",") (trim line)

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
