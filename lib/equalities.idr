include "maybe.idr";
include "vect.idr";

data EqNat : Nat -> Nat -> Set where
    eqO : EqNat O O
  | eqS : EqNat x y -> EqNat (S x) (S y);

eqNat' : (x:Nat) -> (y:Nat) -> Maybe (EqNat x y);
eqNat' O O = Just eqO;
eqNat' (S x) (S y) with eqNat' x y {
  eqNat' (S x) (S x) | Just p = Just (eqS p);
  eqNat' (S x) (S y) | Nothing = Nothing;
}
eqNat' _ _ = Nothing;

eqNat : Nat -> Nat -> Bool;
eqNat x y with eqNat' x y {
  eqNat x x | Just _ = True;
  eqNat x y | Nothing = False;
}

data EqFin : Fin n -> Fin n -> Set where
    eqfO : {k:Nat} -> EqFin {n=S k} fO fO
  | eqfS : {x:Fin n} -> {y:Fin n} -> EqFin x y -> EqFin (fS x) (fS y);

eqFin' : (x:Fin n) -> (y:Fin n) -> Maybe (EqFin x y);
eqFin' fO fO = Just eqfO;
eqFin' (fS x) (fS y) with eqFin' x y {
  eqFin' (fS x) (fS x) | Just p = Just (eqfS p);
  eqFin' (fS x) (fS y) | Nothing = Nothing;
}
eqFin' _ _ = Nothing;

eqFin : Fin n -> Fin n -> Bool;
eqFin x y with eqFin' x y {
  eqFin x x | Just _ = True;
  eqFin x y | Nothing = False;
}

