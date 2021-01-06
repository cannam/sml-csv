
signature CSV_READER = sig

    structure StringMap : ORD_MAP

    type params = CSVSplitter.params
                              
    val foldlStream : params -> (string list * 'a -> 'a) -> 'a -> TextIO.instream -> 'a
    val foldlFile : params -> (string list * 'a -> 'a) -> 'a -> string -> 'a

    (** Open and parse the named file, returning a list of rows, where
        each row consists of a list of columns. Do not treat any row
        or column as a header *)
    val loadFile : params -> string -> string list list

    (** Open and parse the named file, treating the first row as a
        series of column headers, and returning a list of rows, where
        each row consists of a map from column header to value *)
    val loadFileHeaded : params -> string -> string StringMap.map list

    (** Open and parse the named file, treating the first row as a
        series of column headers, and returning a map from column
        header to a list of the values in that column *)
    val loadFileCols : params -> string -> string list StringMap.map
                                        
    (** Open and parse the named file, treating the first column as a
        series of row headers, and returning a map from row header to
        a list of the values in that row *)
    val loadFileRows : params -> string -> string list StringMap.map

    (** Open and parse the named file, treating the first row and the
        first column as headers, and returning a map from row header
        to column header to value *)
    val loadFileRowsAndCols : params -> string -> string StringMap.map StringMap.map

    (** Open and parse the named file, treating the first row and the
        first column as headers, and returning a map from column header
        to row header to value *)
    val loadFileColsAndRows : params -> string -> string StringMap.map StringMap.map

end
