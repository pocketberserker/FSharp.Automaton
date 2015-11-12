module Automaton

type Automaton<'T, 'U> = Step of ('T -> (Automaton<'T, 'U> * 'U))

let run auto b inputs =
  let step (Step f, _) a = f a
  Observable.map (fun (_, y) -> y) (Observable.scan step (auto, b) inputs)

let step a (Step f) = f a

let rec (>>>>) f g =
  Step <| fun a ->
    let (f, b) = step a f
    let (g, c) = step b g
    (f >>>> g, c)

let rec (<<<<) g f =
  Step <| fun a ->
    let (f, b) = step a f
    let (g, c) = step b g
    (g <<<< f, c)

let rec branch f g =
  Step <| fun a ->
    let (f, b) = step a f
    let (g, c) = step a g
    (branch f g, (b, c))

let rec pair f g =
  Step <| fun (a, b) ->
    let (f, c) = step a f
    let (g, d) = step b g
    (pair f g, (c, d))

let rec fst auto =
  Step <| fun (i, ex) ->
    let (f, o) = step i auto
    (fst f, (o, ex))

let rec snd auto =
  Step <| fun (ex, i) ->
    let (f, o) = step i auto
    (snd f, (ex, o))

let rec purely f = Step (fun x -> (purely f, f x))

let merge f = purely (fun (a, b) -> f a b)

let rec loop state auto =
  Step <| fun input ->
    let (auto, (output, state)) = step (input,state) auto
    (loop state auto, output)

let rec combine autos =
  Step <| fun a ->
    let (autos, bs) = List.unzip (List.map (step a) autos)
    (combine autos, bs)

let rec state s f =
  Step <| fun x ->
    let s = f s x
    (state s f, s)

let rec hiddenState s f =
  Step <| fun x ->
    let (s, out) = f x s
    (hiddenState out f, s)

let count<'T> : Automaton<'T, int> = state 0 (fun c _ -> c + 1)

module internal Queue =

  let empty = ([],[])

  let enqueue x (en,de) = (x::en, de)

  let rec dequeue = function
  | ([], []) -> None
  | (en, []) -> dequeue ([], List.rev en)
  | (en, hd::tl) -> Some (hd, (en, tl))


let average (k: int) =

  let stepFull (n: float) (ns, len, sum) =
    match Queue.dequeue ns with
    | None -> (0.0, (ns, len, sum))
    | Some(m, ns) ->
      let sum = sum + n - m
      (sum / float len, (Queue.enqueue n ns, len, sum))

  let step (n: float) (ns, len, sum) =
    if len = k then stepFull n (ns, len, sum)
    else ((sum + n) / (float (len + 1)), (Queue.enqueue n ns, len + 1, sum + n))

  hiddenState (Queue.empty, 0, 0.0) step
