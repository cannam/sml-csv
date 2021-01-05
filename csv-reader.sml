
structure CSVReader : CSV_READER = struct

    structure StringMap = RedBlackMapFn(struct
                                         type ord_key = string
                                         val compare = String.compare
                                         end)

    val split = CSVSplitter.split {
            separator = CSVSplitter.SEPARATOR #",",
            quote_type = CSVSplitter.QUOTE_AUTO,
            escape_type = CSVSplitter.ESCAPE_BACKSLASH
        }
                                            
    fun trim str =
        hd (String.fields (fn x => x = #"\n" orelse x = #"\r") str)

    fun foldlStream f acc stream =
        case TextIO.inputLine stream of
	    SOME line => foldlStream f (f (split (trim line), acc)) stream
          | NONE => acc

    fun foldlFile f acc file =
        let val stream = TextIO.openIn file
            val result = foldlStream f acc stream
        in
            result before TextIO.closeIn stream
        end

    (* ListPair.zip ignores the excess from the tail of the longer
       list (it has to, as it doesn't know what an "empty element"
       looks like) but we want to pad the shorter *)
    fun zip ([], []) = []
      | zip (x::xs, []) = (x, "") :: zip (xs, [])
      | zip ([], y::ys) = ("", y) :: zip ([], ys)
      | zip (x::xs, y::ys) = (x, y) :: zip (xs, ys)

    fun mapRow (headers, fields) =
        List.foldl StringMap.insert' StringMap.empty (zip (headers, fields))

    fun loadFile file =
        rev (foldlFile (op::) [] file)
            
    fun loadFileCols file =
        let val stream = TextIO.openIn file
            val headers =
                case TextIO.inputLine stream of
                    SOME line => split (trim line)
                  | NONE => []
            fun folder (fields, m) =
                List.foldl (fn ((header, field), m) =>
                               StringMap.insert
                                   (m, header,
                                    case StringMap.find (m, header) of
                                        NONE => [field]
                                      | SOME ff => field :: ff))
                           m
                           (zip (headers, fields))
            val result =
                case headers of
                    [] => StringMap.empty
                  | _ => foldlStream folder StringMap.empty stream
        in
            StringMap.map List.rev result
            before TextIO.closeIn stream
        end

    fun loadFileRows file =
        let fun add (row, map) =
                case row of
                    header::rest => StringMap.insert (map, header, rest)
                  | [] => map
        in
            foldlFile add StringMap.empty file
        end

    fun loadFileRowsAndCols file =
        let val stream = TextIO.openIn file
            val headers =
                case TextIO.inputLine stream of
                    NONE => []
                  | SOME line =>
                    case split (trim line) of
                        rowhead::rest => rest
                      | [] => []
            fun add (row, map) =
                case row of
                    [] => map
                  | rowhead::rest => StringMap.insert (map, rowhead,
                                                       mapRow (headers, rest))
            val result = foldlStream add StringMap.empty stream
        in
            result before TextIO.closeIn stream
        end

    fun loadFileColsAndRows file =
        let val stream = TextIO.openIn file
            val headers =
                case TextIO.inputLine stream of
                    SOME line => split (trim line)
                  | NONE => []
            (* first col no good, as we have row-heads: *)
            val headers =
                case headers of
                    [] => []
                  | h::hs => hs
            fun folder (row, m) =
                case row of
                    [] => m
                  | rowhead::rest =>
                    List.foldl (fn ((header, field), m) =>
                                   StringMap.insert
                                       (m, header,
                                        StringMap.insert
                                            (case StringMap.find (m, header) of
                                                 NONE => StringMap.empty
                                               | SOME rm => rm,
                                             rowhead, field)))
                               m
                               (zip (headers, rest))
            val result =
                case headers of
                    [] => StringMap.empty
                  | _ => foldlStream folder StringMap.empty stream
        in
            result before TextIO.closeIn stream
        end
            
end
