exception EMPTY

module type QUEUE = sig
  type 'a queue

  val empty : 'a queue

  val isEmpty : 'a queue -> bool

  val snoc : 'a queue -> 'a -> 'a queue

  val head : 'a queue -> 'a (* raise EMPTY if empty *)

  val tail : 'a queue -> 'a queue (* raise EMPTY if empty *)
end

module BatchedQueue : QUEUE = struct
  type 'a queue =
    { f : 'a list
    ; r : 'a list
    }

  (* Invariant: F is empty only if R is also empty *)

  let empty = { f = []; r = [] }

  let isEmpty { f; _ } =
    match f with
    | [] -> true
    | _ -> false

  let snoc { f; r } x =
    match f with
    | [] -> { f = [ x ]; r = [] }
    | _ -> { f; r = x :: r }

  let head { f; _ } =
    match f with
    | [] -> raise EMPTY
    | x :: _ -> x

  let tail { f; r } =
    match f with
    | [] -> raise EMPTY
    | [ _ ] -> { f = List.rev r; r = [] }
    | _ :: t -> { f = t; r }
end

module BankersQueue = struct
  open Stream

  type 'a queue =
    { f : 'a stream
    ; lenf : int
    ; r : 'a stream
    ; lenr : int
    }

  let empty = { f = Stream.empty; lenf = 0; r = Stream.empty; lenr = 0 }

  let isEmpty { lenf; _ } = lenf = 0

  let queue q =
    let { f; lenf; r; lenr } = q in
    if lenr <= lenf then
      q
    else
      { f = f ++ reverse r; lenf = lenf + lenr; r = Stream.empty; lenr = 0 }

  let snoc { f; lenf; r; lenr } x =
    queue { f; lenf; r = lazy (Cons (x, r)); lenr = lenr + 1 }

  let head { f; _ } =
    match f with
    | (lazy Nil) -> raise EMPTY
    | (lazy (Cons (h, _))) -> h

  let tail { f; lenf; r; lenr } =
    match f with
    | (lazy Nil) -> raise EMPTY
    | (lazy (Cons (_, t))) -> queue { f = t; lenf = lenf - 1; r; lenr }
end