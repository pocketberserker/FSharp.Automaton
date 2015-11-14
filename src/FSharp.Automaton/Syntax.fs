[<AutoOpen>]
module FSharp.Control.AutomatonSyntax

open Automaton

let (>>>>) f g = f >>>> g

let (<<<<) g f = g <<<< f
