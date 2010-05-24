
dumpWords : List String -> IO ();
dumpWords (Cons x xs) = do { putStrLn (x++"."); dumpWords xs; };
dumpWords Nil = return II;

matchInt_test : Int -> String;
matchInt_test 5 = "Five";
matchInt_test 10 = "Ten";
matchInt_test _ = "Something else";

main : IO ();
main = do { let x = "Hello there, world.\nFoo bar.     End.\t";
       	    dumpWords (words x);
	    putStrLn (matchInt_test 5);
	    putStrLn (matchInt_test 6);
	    putStrLn (matchInt_test 10);
	    putStrLn (matchInt_test 11);
	  };
