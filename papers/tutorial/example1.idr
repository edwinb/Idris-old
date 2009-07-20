include "vect.idr";

vadd : (Vect Int n) -> (Vect Int n) -> (Vect Int n);
vadd VNil VNil = VNil;
vadd (VCons x xs) (VCons y ys) = VCons (x+y) (vadd xs ys);

vadd' : (Sigma Nat (Vect Int)) -> (Sigma Nat (Vect Int)) -> 
           (Maybe (Sigma Nat (Vect Int)));
vadd' (Exists {a=n} xs) (Exists {a=m} ys) with (compare n m) {
   vadd' (Exists xs) (Exists ys) | cmpEQ = Just (Exists (vadd xs ys));
                                 | _ = Nothing;
}

readVec : (Vect Int n) -> (IO (Sigma Nat (Vect Int)));
readVec xs = do { putStr "Number: ";
	     	  val <- getInt;
		  putStrLn (showInt val);
	     	  if (val==-1) then (return (Exists xs))
		               else (readVec (VCons val xs));
};

dumpVec : (Vect Int n) -> (IO ());
dumpVec VNil = putStrLn "END";
dumpVec (VCons x xs) = do { putStr (showInt x ++ ", ");
	       	       	    dumpVec xs;
			  };

dumpAns : (Maybe (Sigma Nat (Vect Int))) -> (IO ());
dumpAns Nothing = putStrLn "FAIL!";
dumpAns (Just (Exists {P=Vect Int} xs)) = dumpVec xs;

main : IO ();
main = do { putStrLn "Enter vector 1 (-1 to finish)";
       	    v1 <- readVec VNil;
	    putStrLn "Enter vector 2 (-1 to finish)";
	    v2 <- readVec VNil;
	    dumpAns (vadd' v1 v2);
          };