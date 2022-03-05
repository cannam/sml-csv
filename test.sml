
val testcases_defaultparams = [
    { name = "empty",
      input = "",
      expected = []
    },
    { name = "empties",
      input = ",,",
      expected = ["", "", ""]
    },
    { name = "simple",
      input = "a,b,c,d",
      expected = ["a", "b", "c", "d"]
    },
    { name = "dquoted",
      input = "a,\"b,c\",d",
      expected = ["a", "b,c", "d"]
    },
    { name = "squoted",
      input = "a,'b,c',d",
      expected = ["a", "b,c", "d"]
    },
    { name = "drunon",
      input = "a,\"b,c\"d,e",
      expected = ["a", "b,cd", "e"]
    },
    { name = "srunon",
      input = "a,'b,c'd,e",
      expected = ["a", "b,cd", "e"]
    },
    { name = "descaped",
      input = "a,\"b,c\\\",d\"",
      expected = ["a", "b,c\",d"]
    },
    { name = "sescaped",
      input = "a,'b,c\\',d'",
      expected = ["a", "b,c',d"]
    },
    { name = "ddescaped",
      input = "a,\"b,c\"\",d\"",
      expected = ["a", "b,c\",d"]
    },
    { name = "sdescaped",
      input = "a,'b,c'',d'",
      expected = ["a", "b,c',d"]
    },
    { name = "dnested",
      input = "a,\"b,c',d\",e",
      expected = ["a", "b,c',d", "e"]
    },
    { name = "snested",
      input = "a,'b,c\",d',e",
      expected = ["a", "b,c\",d", "e"]
    },
    { name = "dnested2",
      input = "aa,\"bb,cc',dd\"",
      expected = ["aa", "bb,cc',dd"]
    },
    { name = "snested2",
      input = "aa,'bb,cc\",dd'",
      expected = ["aa", "bb,cc\",dd"]
    },
    { name = "dnested3",
      input = "\"aa,bb,cc'\",dd",
      expected = ["aa,bb,cc'","dd"]
    },
    { name = "snested3",
      input = "'aa,bb,cc\"',dd",
      expected = ["aa,bb,cc\"","dd"]
    },
    { name = "dnested4",
      input = "\"aa,'bb,cc',dd\"",
      expected = ["aa,'bb,cc',dd"]
    },
    { name = "snested4",
      input = "'aa,\"bb,cc\",dd'",
      expected = ["aa,\"bb,cc\",dd"]
    },
    { name = "qquoted",
      input = "a'a,'bb',\\\"cc\",dd\\\"",
      expected = ["a'a", "bb", "\"cc\"", "dd\""]
    },
    { name = "qspace",
      input = "\"a,a\" \"b,b\" c,d\"",
      (* Can't start a quote in the middle of a bare field - they are
         handled only if the first character in the field is a
         quote. Otherwise we'd have trouble with apostrophes etc *)
      expected = ["a,a \"b","b\" c", "d\""]
    },
    { name = "multisep",
      input = ",,a'a,\\',,,,,,,,,'bb',,,,',,,,,,\\\"cc\",',dd\\\",'",
      expected = ["", "", "a'a", "'", "", "", "", "", "", "",
                  "", "", "bb", "", "", "", ",,,,,,\"cc\",", "dd\"", "'"]
    }    
]

val testcases_spacechar_separator = [
    { name = "empty",
      input = "",
      expected = []
    },
    { name = "empties",
      input = "  ",
      expected = ["", "", ""]
    },
    { name = "simple",
      input = "a b c d",
      expected = ["a", "b", "c", "d"]
    },
    { name = "multisep",
      input = "  a'a \\'     " ^ str (chr 9) (* tab *) ^
              "    'bb'    '      \\\"cc\" ' dd\\\" '",
      expected = ["", "", "a'a", "'", "", "", "", "", str (chr 9), "",
                  "", "", "bb", "", "", "", "      \"cc\" ", "dd\"", "'"]
    }
]

val testcases_whitespace_separator = [
    { name = "empty",
      input = "",
      expected = []
    },
    { name = "empties",
      input = "  ",
      expected = ["", ""]
    },
    { name = "simple",
      input = "a b c d",
      expected = ["a", "b", "c", "d"]
    },
    { name = "multisep",
      input = "  a'a \\'     " ^ str (chr 9) (* tab *) ^
              "    'bb'    '      \\\"cc\" ' dd\\\" '",
      expected = ["", "a'a", "'", "bb", "      \"cc\" ", "dd\"", "'"]
    }
]

fun report converter (obtained, expected) =
    print ("--- Expected " ^ (converter expected)
           ^ "\n--- Obtained " ^ (converter obtained) ^ "\n")
                                    
fun checkWith converter comparator (a, b) =
    if comparator (a, b) then true
    else (report converter (a, b); false)

fun check converter (a, b) = checkWith converter (op=) (a, b)

fun checkListsWith converter comparator (a, b) =
    let fun checkLists' ([], []) = true
          | checkLists' (a', b') =
            checkWith converter comparator (hd a', hd b') andalso
            checkLists' (tl a', tl b')
    in
        if (List.length a <> List.length b)
        then 
            (print ("--- Lists have differing lengths (expected " ^
                    (Int.toString (List.length b)) ^
                    ", obtained " ^
                    (Int.toString (List.length a)) ^ ": [" ^
		    (String.concatWith "," (List.map converter a)) ^
                    "])\n");
             false)
        else
            checkLists' (a, b)
    end

fun checkLists converter (a, b) = checkListsWith converter (op=) (a, b)

val quoting_tests =
    List.concat
        (map (fn (sepname, separator, testcases) =>
                 map (fn { name, input, expected } =>
                         ("quoting-" ^ sepname ^ "-" ^ name,
                          fn () => 
                             let val obtained =
                                     CSVSplitter.split {
                                         separator = separator,
                                         quote_type = CSVSplitter.QUOTE_AUTO,
                                         escape_type = CSVSplitter.ESCAPE_AUTO
                                     } input
                                 fun conc ss = 
                                     String.concatWith
                                         "," (map (fn s => "\"" ^ s ^ "\"") ss)
                             in
                                 if obtained = expected
                                 then true
                                 else (print ("In test " ^ name ^ ":\n" ^
                                              " -> for input: \"" ^ input ^ "\"\n" ^
                                              " -> expected: [" ^ conc expected ^ "]\n" ^
                                              " -> obtained: [" ^ conc obtained ^ "]\n");
                                       false)
                             end))
                     testcases)
             [("comma",
               CSVSplitter.SEPARATOR #",",
               testcases_defaultparams),
              ("spacechar",
               CSVSplitter.SEPARATOR #" ",
               testcases_spacechar_separator),
              ("whitespace",
               CSVSplitter.SEPARATOR_WHITESPACE,
               testcases_whitespace_separator)
             ])
        
structure M = CSVReader.StringMap

fun testfile () =
    let val filename = "testfile.csv"
        val candidateDirectories = ["sml-csv", "."]
	in
	    case foldl (fn (candidate, SOME acceptable) => SOME acceptable
                         | (candidate, NONE) =>
                           if OS.FileSys.access (candidate ^ "/" ^ filename,
                                                 [OS.FileSys.A_READ])
			   then SOME candidate
			   else NONE)
		       NONE
		       candidateDirectories of
		NONE => raise Fail
                              ("Test file not found (candidate dirs: "
			       ^ String.concatWith ", " candidateDirectories
                               ^ ")")
	      | SOME acceptable => acceptable ^ "/" ^ filename
    end
                   
(* testfile looks like this:

,Col1,Col2
Row1,A
Row2,X,Y

*)
                   
val checkStringLists = checkLists (fn s => s)
val checkStrings = check (fn s => s)
                   
val loading_tests = [
    
    ("loading-plain",
     fn () => 
        let val ll = CSVReader.loadFile CSVSplitter.defaultParams (testfile ())
        in
            check Int.toString (length ll, 3) andalso
            checkStringLists (hd ll, ["", "Col1", "Col2"]) andalso
            checkStringLists (hd (tl ll), ["Row1", "A"]) andalso
            checkStringLists (hd (tl (tl ll)), ["Row2", "X", "Y"])
        end),
    
    ("loading-headed",
     fn () => 
        let val ml = CSVReader.loadFileHeaded CSVSplitter.defaultParams (testfile ())
        in
            check Int.toString (length ml, 2) andalso
            checkStringLists (M.listKeys (hd ml), ["", "Col1", "Col2"]) andalso
            checkStringLists (M.listKeys (hd (tl ml)), ["", "Col1", "Col2"]) andalso
            checkStrings (M.lookup (hd ml, ""), "Row1") andalso
            checkStrings (M.lookup (hd ml, "Col1"), "A") andalso
            checkStrings (M.lookup (hd ml, "Col2"), "") andalso
            checkStrings (M.lookup (hd (tl ml), ""), "Row2") andalso
            checkStrings (M.lookup (hd (tl ml), "Col1"), "X") andalso
            checkStrings (M.lookup (hd (tl ml), "Col2"), "Y") 
        end),
    
    ("loading-cols",
     fn () =>
        let val lm = CSVReader.loadFileCols CSVSplitter.defaultParams (testfile ())
            val kk = M.listKeys lm
        in
            checkStringLists (kk, ["", "Col1", "Col2"]) andalso
            checkStringLists (M.lookup (lm, ""), ["Row1", "Row2"]) andalso
            checkStringLists (M.lookup (lm, "Col1"), ["A", "X"]) andalso
            checkStringLists (M.lookup (lm, "Col2"), ["", "Y"])
        end
    ),
    
    ("loading-rows",
     fn () =>
        let val lm = CSVReader.loadFileRows CSVSplitter.defaultParams (testfile ())
            val kk = M.listKeys lm
        in
            checkStringLists (kk, ["", "Row1", "Row2"]) andalso
            checkStringLists (M.lookup (lm, ""), ["Col1", "Col2"]) andalso
            checkStringLists (M.lookup (lm, "Row1"), ["A"]) andalso
            checkStringLists (M.lookup (lm, "Row2"), ["X", "Y"])
        end
    ),

    ("loading-rowscols",
     fn () =>
        let val mm = CSVReader.loadFileRowsAndCols CSVSplitter.defaultParams (testfile ())
            val kk = M.listKeys mm
        in
            checkStringLists (kk, ["Row1", "Row2"]) andalso
            List.all (fn k =>
                         checkStringLists (M.listKeys (M.lookup (mm, k)),
                                           ["Col1", "Col2"]))
                     kk andalso
            checkStrings (M.lookup (M.lookup (mm, "Row1"), "Col1"), "A") andalso
            checkStrings (M.lookup (M.lookup (mm, "Row1"), "Col2"), "") andalso
            checkStrings (M.lookup (M.lookup (mm, "Row2"), "Col1"), "X") andalso
            checkStrings (M.lookup (M.lookup (mm, "Row2"), "Col2"), "Y")
        end),
            
    ("loading-colsrows",
     fn () =>
        let val mm = CSVReader.loadFileColsAndRows CSVSplitter.defaultParams (testfile ())
            val kk = M.listKeys mm
        in
            checkStringLists (kk, ["Col1", "Col2"]) andalso
            List.all (fn k =>
                         checkStringLists (M.listKeys (M.lookup (mm, k)),
                                           ["Row1", "Row2"]))
                     kk andalso
            checkStrings (M.lookup (M.lookup (mm, "Col1"), "Row1"), "A") andalso
            checkStrings (M.lookup (M.lookup (mm, "Col1"), "Row2"), "X") andalso
            checkStrings (M.lookup (M.lookup (mm, "Col2"), "Row1"), "") andalso
            checkStrings (M.lookup (M.lookup (mm, "Col2"), "Row2"), "Y")
        end)
]

val csv_tests = quoting_tests @ loading_tests

fun main () =
    let val result =
            foldl (fn ((name, test), success) =>
                      if test ()
                      then success
                      else false)
                  true
                  csv_tests
    in
        if result
        then print "All tests passed\n"
        else print "Some tests failed\n"
    end
