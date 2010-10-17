data Bool = True | False;

not : Bool -> Bool;
not True = False;
not False = True;

%transform not (not ?x) => ?x;

if_then_else : Bool -> |(t:A) -> |(e:A) -> A;
if_then_else True t f = t;
if_then_else False t f = f;

data so : Bool -> Set where oh : so True;

infixl 4 &&,||;

(||) : Bool -> Bool -> Bool;
(||) False x = x;
(||) True _ = True;

(&&) : Bool -> Bool -> Bool;
(&&) True x = x;
(&&) _ _ = False;
