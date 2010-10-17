data Tactic : Set where
    TFill : {a:Set} -> a -> Tactic
  | TRefine : String -> Tactic
  | TTrivial : Tactic
  | TDecide : {a:Set} -> Maybe a -> Tactic
  | TSearchContext : Tactic
  | TTry : Tactic -> Tactic -> Tactic
  | TSeq : Tactic -> Tactic -> Tactic
  | TThen : Tactic -> Tactic -> Tactic
  | TThenAll : Tactic -> Tactic -> Tactic
  | TFail : String -> Tactic;
