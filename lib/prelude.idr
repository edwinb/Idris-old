-- malloc evaluates an expression using a manual allocator, allocating 'bytes'
-- Needs to be compiled specially, naturally.

malloc : Int -> a -> a;
malloc bytes val = val;

%freeze malloc;

-- Used by the 'believe' tactic to make a temporary proof. Programs
-- using this are not to be trusted! (Or maybe use externally trusted code)
-- Generate a refl so that __eq_repl can reduce.

__Suspend_Disbelief : {A:Set} -> (m:A) -> (n:A) -> (n = m);
__Suspend_Disbelief m n = __Prove_Anything _ _ (refl n);

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

mod : Int -> Int -> Int; [inline]
mod x y = __modInt x y;

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

exp : Float -> Float; [inline]
exp x = __floatExp x;

log : Float -> Float; [inline]
log x = __floatLog x;

%lib "--lm"

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

