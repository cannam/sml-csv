
structure CSVReader : CSV_READER = struct

    type params = CSVSplitter.params

    structure StringMap = RedBlackMapFn(struct
                                         type ord_key = string
                                         val compare = String.compare
                                         end)
                                            
    fun trim str =
        hd (String.fields (fn x => x = #"\n" orelse x = #"\r") str)

    fun split params =
        fn line => CSVSplitter.split params (trim line)

    fun foldlStream params f acc stream =
        case TextIO.inputLine stream of
	    SOME line =>
            foldlStream params f (f (split params line, acc)) stream
          | NONE => acc

    fun foldlFile params f acc file =
        let val stream = TextIO.openIn file
            val result = foldlStream params f acc stream
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

    fun loadFile params file =
        rev (foldlFile params (op::) [] file)
            
    fun loadFileCols params file =
        let val stream = TextIO.openIn file
            val split = split params
            val headers =
                case TextIO.inputLine stream of
                    SOME line => split line
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
                  | _ => foldlStream params folder StringMap.empty stream
        in
            StringMap.map List.rev result
            before TextIO.closeIn stream
        end

    fun loadFileRows params file =
        let fun add (row, map) =
                case row of
                    header::rest => StringMap.insert (map, header, rest)
                  | [] => map
        in
            foldlFile params add StringMap.empty file
        end

    fun loadFileRowsAndCols params file =
        let val stream = TextIO.openIn file
            val split = split params
            val headers =
                case TextIO.inputLine stream of
                    NONE => []
                  | SOME line =>
                    case split line of
                        rowhead::rest => rest
                      | [] => []
            fun add (row, map) =
                case row of
                    [] => map
                  | rowhead::rest => StringMap.insert (map, rowhead,
                                                       mapRow (headers, rest))
            val result = foldlStream params add StringMap.empty stream
        in
            result before TextIO.closeIn stream
        end

    fun loadFileColsAndRows params file =
        let val stream = TextIO.openIn file
            val split = split params
            val headers =
                case TextIO.inputLine stream of
                    SOME line => split line
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
                  | _ => foldlStream params folder StringMap.empty stream
        in
            result before TextIO.closeIn stream
        end
            
end
