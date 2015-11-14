namespace FSharp.Control

type Automaton<'T, 'U> = Step of ('T -> (Automaton<'T, 'U> * 'U))
