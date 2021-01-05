
signature CSV_READER = sig

    structure StringMap : ORD_MAP

    val foldlStream : (string list * 'a -> 'a) -> 'a -> TextIO.instream -> 'a
    val foldlFile : (string list * 'a -> 'a) -> 'a -> string -> 'a

    (** Open and parse the named file, returning a list of rows, where
        each row consists of a list of columns. Do not treat any row
        or column as a header *)
    val loadFile : string -> string list list

    (** Open and parse the named file, treating the first row as a
        series of column headers, and returning a map from column
        header to a list of the values in that column *)
    val loadFileCols : string -> string list StringMap.map
                                        
    (** Open and parse the named file, treating the first column as a
        series of row headers, and returning a map from row header to
        a list of the values in that row *)
    val loadFileRows : string -> string list StringMap.map

    (** Open and parse the named file, treating the first row and the
        first column as headers, and returning a map from row header
        to column header to value *)
    val loadFileRowsAndCols : string -> string StringMap.map StringMap.map

    (** Open and parse the named file, treating the first row and the
        first column as headers, and returning a map from column header
        to row header to value *)
    val loadFileColsAndRows : string -> string StringMap.map StringMap.map

end
