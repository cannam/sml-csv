
signature CSV_READER = sig

    structure Map : ORD_MAP

    val foldlStream : (string list * 'a -> 'a) -> 'a -> TextIO.instream -> 'a
    val foldlStreamCols : (string Map.map * 'a -> 'a) -> 'a -> (TextIO.instream * string list) -> 'a

    val foldlFile : (string list * 'a -> 'a) -> 'a -> string -> 'a
    val foldlFileCols : (string Map.map * 'a -> 'a) -> 'a -> string -> 'a

    val loadFile : string -> string list list
    val loadFileCols : string -> string Map.map list
    val loadFileRows : string -> string list Map.map
    val loadFileRowsAndCols : string -> string Map.map Map.map
                                                                             
end
