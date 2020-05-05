
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
      expected = ["a", "b,c\"d"]
    },
    { name = "sescaped",
      input = "a,'b,c\\',d'",
      expected = ["a", "b,c\"d"]
    },
    { name = "ddescaped",
      input = "a,\"b,c\"\",d\"",
      expected = ["a", "b,c\"d"]
    },
    { name = "sdescaped",
      input = "a,'b,c'',d'",
      expected = ["a", "b,c'd"]
    },
    { name = "dnested",
      input = "a,\"b,c',d\",e",
      expected = ["a", "b,c'd", "e"]
    },
    { name = "snested",
      input = "a,'b,c\",d',e",
      expected = ["a", "b,c\"d", "e"]
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
      input = "\"a,a\" \"b,b\" \"c,d\"",
      (* Can't start a quote in the middle of a bare field - they are
         handled only if the first character in the field is a
         quote. Otherwise we'd have trouble with apostrophes etc *)
      expected = ["a,a \"b,b\" \"c", "d\""]
    },
    { name = "multispace",
      input = ",,a'a,\\',,,,,,,,,'bb',,,,',,,,,,\\\"cc\",',dd\\\",'",
      expected = ["", "", "a'a", "'", "", "", "", "", "", "",
                  "", "", "bb", "", "", "", ",,,,,,\"cc\",", "dd\"", "'"]
    }    
]

fun test () =
    case (foldl (fn ({ name, input, expected }, failures) =>
                    let val obtained =
                            CSVSplitter.split {
                                separator = CSVSplitter.SEPARATOR #",",
                                quote_type = CSVSplitter.QUOTE_AUTO,
                                escape_type = CSVSplitter.ESCAPE_AUTO
                            } input
                        fun conc ss = 
                            String.concatWith "," (map (fn s => "\"" ^ s ^ "\"") ss)
                    in
                        if obtained = expected
                        then (print ("PASS: " ^ name ^ "\n"); failures)
                        else (print ("In test " ^ name ^ ":\n" ^
                                     " -> for input: \"" ^ input ^ "\"\n" ^
                                     " -> expected: [" ^ conc expected ^ "]\n" ^
                                     " -> obtained: [" ^ conc obtained ^ "]\n");
                              print ("FAIL: " ^ name ^ "\n");
                              failures + 1)
                    end)
                0
                testcases_defaultparams) of
        0 => print "All tests passed\n"
      | n => (print (Int.toString n ^ " test(s) failed!\n");
              raise Fail "Some tests failed")

fun main () = test ()
                   
