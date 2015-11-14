namespace FSharp.Control.Tests

open System.Reactive.Linq
open FSharp.Control
open Persimmon
open UseTestNameByReflection

module AutomatonTest =

  let run auto b input = (Automaton.run auto b input).Wait()

  let ``subscribe count`` = test {
    let count = Observable.Range(0, 2) |> run Automaton.count 0
    do! assertEquals 2 count
  }

  let ``subscribe average`` = test {
    let auto = Automaton.purely float >>>> Automaton.average 10
    let average = Observable.Range(0, 10) |> run auto 0.0
    do! assertEquals 4.5 average
  }

  let ``choose and subscribe count`` = test {
    let listed = [ 0 .. 2 .. 10 ]
    let choosed = Automaton.choose (fun x -> List.tryFind ((=) x) listed) -1 Automaton.count
    let count = Observable.Range(0, 10) |> run choosed 0
    do! assertEquals 5 count
  }

  let ``filter and subscribe count`` = test {
    let filtered = Automaton.filter (fun x -> x % 2 = 0) -1 Automaton.count
    let count = Observable.Range(0, 10) |> run filtered 0
    do! assertEquals 5 count
  }
