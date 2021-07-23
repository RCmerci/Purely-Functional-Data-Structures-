type 'a stream_cell =
  | Nil
  | Cons of ('a * 'a stream)

and 'a stream = 'a stream_cell Lazy.t

let rec ( ++ ) (s : 'a stream) (t : 'a stream) =
  lazy
    (match s with
    | (lazy Nil) -> Lazy.force t
    | (lazy (Cons (h, t'))) -> Cons (h, t' ++ t))

let rec drop n s =
  match (n, s) with
  | 0, s -> s
  | _, (lazy Nil) -> s
  | n, (lazy (Cons (_, t))) -> drop (n - 1) t

let reverse s =
  let rec reverse' = function
    | (lazy Nil), r -> r
    | (lazy (Cons (h, t))), r -> reverse' (t, lazy (Cons (h, r)))
  in
  reverse' (s, lazy Nil)

let empty = lazy Nil

let s1 = lazy (Cons (1, lazy Nil))

let s2 = lazy (Cons (2, lazy (Cons (3, lazy Nil))))
