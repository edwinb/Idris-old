-- System/OS interaction functions

namespace System {

  numArgs = mkForeign (FFun "epic_numArgs" [] FInt); [%eval]
  getArgn = mkForeign (FFun "epic_getArg" [FInt] FStr); [%eval]

  getArgs' : List String -> Int -> IO (List String);
  getArgs' acc 0 = return acc;
  getArgs' acc n = do { arg <- getArgn (n-1);
                        getArgs' (Cons arg acc) (n-1); 
                      };

  getArgs : IO (List String);
  getArgs = do { num <- numArgs;
                 getArgs' [] num; };

}