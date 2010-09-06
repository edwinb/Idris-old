-- malloc evaluates an expression using a manual allocator, allocating 'bytes'
-- Needs to be compiled specially, naturally.

malloc : Int -> a -> a;
malloc bytes val = val;

%freeze malloc;

flip : (a -> b -> c) -> b -> a -> c;
flip f x y = f y x;

infixl 5 ==, /=, ==.;
infixl 6 <, <=, >, >=, <., <=., >., >=.;
infixl 7 <<, >>;
infixl 8 +,-,++,+.,-.;
infixl 9 *,/,*.,/.;

-- Integer primitives

(+) : Int -> Int -> Int; [inline]
(+) x y = __addInt x y;

(-) : Int -> Int -> Int; [inline]
(-) x y = __subInt x y;

(*) : Int -> Int -> Int; [inline]
(*) x y = __mulInt x y;

(/) : Int -> Int -> Int; [inline]
(/) x y = __divInt x y;

(<) : Int -> Int -> Bool; [inline]
(<) x y = __intlt x y;

(<=) : Int -> Int -> Bool; [inline]
(<=) x y = __intleq x y;

(>) : Int -> Int -> Bool; [inline]
(>) x y = __intgt x y;

(>=) : Int -> Int -> Bool; [inline]
(>=) x y = __intgeq x y;

(==) : Int -> Int -> Bool; [inline]
(==) x y = __eq x y;

(/=) : Int -> Int -> Bool; [inline]
(/=) x y = not (__eq x y);

(<<) : Int -> Int -> Int; [inline]
(<<) x y = __shl x y;

(>>) : Int -> Int -> Int; [inline]
(>>) x y = __shr x y;

-- Floating point primitives

(+.) : Float -> Float -> Float; [inline]
(+.) x y = __addFloat x y;

(-.) : Float -> Float -> Float; [inline]
(-.) x y = __subFloat x y;

(*.) : Float -> Float -> Float; [inline]
(*.) x y = __mulFloat x y;

(/.) : Float -> Float -> Float; [inline]
(/.) x y = __divFloat x y;

(<.) : Float -> Float -> Bool; [inline]
(<.) x y = __floatlt x y;

(<=.) : Float -> Float -> Bool; [inline]
(<=.) x y = __floatleq x y;

(>.) : Float -> Float -> Bool; [inline]
(>.) x y = __floatgt x y;

(>=.) : Float -> Float -> Bool; [inline]
(>=.) x y = __floatgeq x y;

(==.) : Float -> Float -> Bool; [inline]
(==.) x y = __feq x y;

-- String primitives

(++) : String -> String -> String; [inline]
(++) x y = __concat x y;


-- Function composition

infixl 9 .;

(.) : (b -> c) -> (a -> b) -> a -> c;
(.) f g x = f (g x);

fst : (a & b) -> a; [inline]
fst (x, y) = x;

snd : (a & b) -> b; [inline]
snd (x, y) = y;

include "nat.idr";
include "maybe.idr";
include "io.idr";
include "either.idr";
include "tactics.idr";
include "vect.idr";
include "string.idr";

