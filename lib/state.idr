namespace State {

  State : Set -> Set -> Set;
  State s a = s -> (a & s);

  bind : State s a -> (a -> State s b) -> State s b;
  bind fa k state with fa state {
     | (av, state') = k av state';
  }

  ret : a -> State s a;
  ret av state = (av, state);

  get : State s s;
  get state = (state, state);

  put : s -> State s ();
  put state _ = (II, state);

  runState : State s a -> s -> (a & s);
  runState s init = s init;

  execState : State s a -> s -> s;
  execState s init = snd (runState s init);

  evalState : State s a -> s -> a;
  evalState s init = fst (runState s init);

}
