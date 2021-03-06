include "bool.idr"; 

data __Unit = II;
data __Empty = ;

data Sigma : (A:Set)->(P:A->Set)->Set where
   Exists : {P:A->Set} -> (a:A) -> P a -> Sigma A P;

getSigIdx : {P:a->Set} ->  (s:Sigma a P) -> a;
getSigIdx (Exists a v) = a;

getSigVal : {P:a->Set} -> (s:Sigma a P) -> P (getSigIdx s);
getSigVal (Exists a v) = v;

data Pair a b = mkPair a b;

id : a -> a;
id x = x;

rewrite : {B:Set} -> {A:B->Set} -> A m -> (m=n) -> A n;
rewrite {B} t (refl m) = t;

-- This way is needed for Ivor's rewriting tactic

__eq_repl : (A:Set)->(x:A) -> (y:A) -> (q:(x=y)) -> (P:(m:A)->Set) -> (p:P x) -> (P y);
__eq_repl A x x (refl _) P p = p;

__eq_sym : (A:Set) -> (a:A) -> (b:A) -> (p:(a=b)) -> (b=a);
__eq_sym A a a (refl _) = refl _;

-- For proofs which should not be stored at run-time. Programs can
-- construct objects of type Proof A, and manipulate them,
-- but not inspect them. 'Proof' is treated as collapsible.

data Proof : (A:Set) -> Set where
  __mkProof : (a:A) -> Proof A;

prove : (a:A) -> Proof A;
prove x = __mkProof x;

-- This is the only function allowed to manipulate proofs.
-- It'd be easier if we could hide '__mkProof'!

proof_bind : Proof A -> (A -> Proof B) -> Proof B;
proof_bind (__mkProof a) p = p a;

