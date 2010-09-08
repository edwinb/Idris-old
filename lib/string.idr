include "list.idr";

strLen: String -> Int; [inline]
strLen str = __strlen str;

strEq: String -> String -> Bool; [inline]
strEq s1 s2 = __strEq s1 s2;

charEq : Char -> Char -> Bool; [inline]
charEq c1 c2 = __charToInt c1 == __charToInt c2;

concat: String -> String -> String; [inline]
concat s1 s2 = __concat s1 s2;

strNull: String -> Bool; [inline]
strNull s = strEq s "";

strHead: String -> Maybe Char; [inline]
strHead s = if (strNull s) then Nothing else (Just (__strHead s));

strTail: String -> Maybe String; [inline]
strTail s = if (strNull s) then Nothing else (Just (__strTail s));

strRev : String -> String; [inline]
strRev s = __strRev s;

substr : String -> Int -> Int -> String; [inline]
substr s start len = __substr s start len;

strFind : String -> Char -> Int; [inline]
strFind s c = __strFind s c;

strSplit : Char -> String -> (String & String);
strSplit c str = let idx = strFind str c in
	 if (idx == (-1)) then (str, "") else
	 (substr str 0 idx, substr str (idx+1) (strLen str - (idx+1)));

-- Some more, faster, string manipulations

strHead' : (x:String) -> (so (not (strNull x))) -> Char;
strHead' x p = __strHead x;

strTail' : (x:String) -> (so (not (strNull x))) -> String;
strTail' x p = __strTail x;

strCons: Char -> String -> String; [inline]
strCons c s = __strCons c s;

strUncons: String -> Maybe (Char & String); [inline]
strUncons s with (strHead s, strTail s) {
  | (Just h,  Just t)  = Just (h, t);
  | (Nothing, Nothing) = Nothing;
}

{-- A view of strings, for better, faster, pattern matching --}

data StrM : String -> Set where
   StrNil : StrM ""
 | StrCons : (x:Char) -> (xs:String) -> StrM (strCons x xs);

strM : (x:String) -> StrM x;
strM x with choose (strNull x) {
   | Left p ?= StrCons (strHead' x p) (strTail' x p);     [strMleft]
   | Right p ?= StrNil;                                   [strMright]
}

strMright proof {
  %intro;
  %believe value; -- it's a primitive operation, we have to believe it!
  %qed;
};

strMleft proof {
  %intro;
  %believe value; -- it's a primitive operation, we have to believe it!
  %qed;
};


charAt: Int -> String -> Maybe Char; [inline]
charAt x str =
  if (strLen str > x && x >= 0) then (Just (__strgetIdx str x))
                                else Nothing;

showInt: Int -> String; [inline]
showInt x = __toString x;

readInt: String -> Maybe Int;
readInt str = let x = __toInt str
              in  if (strEq str (showInt x))
                     then (Just x)
                     else Nothing;

showNat: Nat -> String;
showNat n = __toString (natToInt n);

readNat: String -> Maybe Nat;
readNat str with readInt str {
  | Just x  = if (x >= 0) then (Just (intToNat x)) else Nothing;
  | Nothing = Nothing;
}

strToList: String -> List Char;
strToList s with strUncons s {
  | Just (h, t) = Cons h (strToList t);
  | Nothing     = Nil;
}

listToStr: List Char -> String;
listToStr = foldr strCons "";

-- TODO if the change to the parser breaks things, the sigma pattern will
--      need parens around
strToVect: String -> (n ** Vect Char n);
strToVect s with strUncons s {
    | Just (c, cs) with strToVect cs {
    | <| cs' |> = <| c :: cs' |>;
  }
  | Nothing      = <| VNil |>;
}

vectToStr: Vect Char n -> String;
vectToStr (h :: t) = strCons h (vectToStr t);
vectToStr VNil     = "";


data StrCmp = StrLT | StrEQ | StrGT;
strCmp: String -> String -> StrCmp;
strCmp s t =
  if      (__strLT s t) then StrLT
  else if (strEq s t)   then StrEQ
  else                       StrGT;

strSpan' : (Char -> Bool) -> String -> String -> (String & String);
strSpan' p str acc with strM str {
  strSpan' p "" acc | StrNil 
         = (strRev acc, "");
  strSpan' p (strCons c cs) acc | StrCons _ _
         = if (p c) then (strSpan' p cs (strCons c acc))
    	      	    else (strRev acc, (strCons c cs));
}

strSpan : (Char -> Bool) -> String -> (String & String);
strSpan p str = strSpan' p str "";

-- TODO: A collection of these in a Char library.

isSpace : Char -> Bool;
isSpace ' ' = True;
isSpace '\t' = True;
isSpace '\r' = True;
isSpace '\n' = True;
isSpace _ = False;

isNL : Char -> Bool;
isNL '\r' = True;
isNL '\n' = True;
isNL _ = False;

isAlpha : Char -> Bool;
isAlpha x = let a = __charToInt 'a' in
            let z = __charToInt 'z' in
            let A = __charToInt 'A' in
            let Z = __charToInt 'Z' in
	    let x = __charToInt x in
	    (x >= a && x <= z) || (x >= A && x <= Z);

isDigit : Char -> Bool;
isDigit x = let a = __charToInt '0' in
            let z = __charToInt '9' in
	    let x = __charToInt x in
	    (x >= a && x <= z);

words : String -> List String;
words str with (strSpan (not . isSpace) str) {
   | ("", "") = Nil;
   | (word, rest) with choose (strNull rest) {
       | Left rp with choose (strNull word) {
         | Left wp = Cons word (words (strTail' rest rp));
         | Right wp = words (strTail' rest rp);
         }
       | Right rp = Cons word Nil;
   }
}

lines : String -> List String;
lines str with (strSpan (not . isNL) str) {
   | ("", "") = Nil;
   | (line, rest) with choose (strNull rest) {
       | Left rp with choose (strNull line) {
         | Left wp = Cons line (lines (strTail' rest rp));
         | Right wp = lines (strTail' rest rp);
         }
       | Right rp = Cons line Nil;
   }
}

-- Generic version of lines/words

splitBy : (Char -> Bool) -> String -> List String;
splitBy p str with (strSpan (not . p) str) {
   | ("", "") = Nil;
   | (word, rest) with choose (strNull rest) {
       | Left rp with choose (strNull word) {
         | Left wp = Cons word (splitBy p (strTail' rest rp));
         | Right wp = splitBy p (strTail' rest rp);
         }
       | Right rp = Cons word Nil;
   }
}

unlines : List String -> String;
unlines Nil = "";
unlines (Cons x xs) = x ++ "\n" ++ unlines xs;

unwords : List String -> String;
unwords Nil = "";
unwords (Cons x Nil) = x;
unwords (Cons x xs) = x ++ " " ++ unwords xs;

trimLeft : String -> String;
trimLeft str with (strSpan isSpace str) {
   | (spcs, rest) = rest;
}

trimRight : String -> String;
trimRight x = strRev (trimLeft (strRev x));

trim : String -> String;
trim x = trimLeft (strRev (trimLeft (strRev x)));

mapStr : (Char -> Char) -> String -> String;
mapStr f str with strM str {
 mapStr f ""             | StrNil      = "";
 mapStr f (strCons c cs) | StrCons _ _ = strCons (f c) (mapStr f cs);
}

toLower : Char -> Char;
toLower x = let xi = __charToInt x in
	    let Ai = __charToInt 'A' in
	    let ai = __charToInt 'a' in
	    let Zi = __charToInt 'Z' in
	    if (xi>=Ai && xi<=Zi) then (__intToChar (xi+ai-Ai)) else x;

