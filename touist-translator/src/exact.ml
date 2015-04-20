(* Represents one combination by the structure (combination, others) such as
  - `combination` is the list of the terms such as there are exactly N elmts
  - `others` is the rest of the elemts that have not been picked for that
  combination *)
type ('a) fullcombination = 'a list * 'a list;;

(*`_exact` is a sub-function for `exact`.
  It gets all the possible combinations of n elements from `succ`.
  `pred` should be initialized at ([],[]).
  Returns a list of `fullcombination`. This function is used by `exact` *)
let rec _exact (pred:'a fullcombination) (succ:'a list) (n: int)
  : 'a fullcombination list =
  let (combination,others) = pred in (* pos and others are lists *)
  match succ with
  | [] -> []
  | head::tail -> if n > 1 then
    (_exact (head::combination,others) tail (n-1))
      @(_exact (combination,head::others) tail n)
  else if n = 1 then
    (head::combination,others@tail)
      ::(_exact (combination,head::others) tail 1)
  else (* The n=0 case *)
    (combination,(head::others)@tail)
      ::(_exact (combination,head::others) tail 0);;

(*`exact`: finds every n-elements combinations
  and returns a list of `fullcombination`.
  Ex: exact [1;2;3] 2 => [[[1;2] ; 3] ; [[1;3] ; 2] ; [[2;3] ; 1]]
  Note: `fullcombination` is a structure (combination,others) such as
  - `combination`: list of `n` elements defining a combination of n elmts
  - `others` is a list of the other elements not picked for this combination*)
let exact (elements:'a list) (n: int) : 'a fullcombination list =
  (_exact ([],[]) elements n);;

(*`atmost`: finds every m-elements combination with m going from n to 0
  and returns a list of `fullcombination`.
  Note: `fullcombination` is a structure (combination,others) such as
  - `combination`: list of `n` elements defining a combination of n or less elmts
  - `others` is a list of the other elements not picked for this combination*)
let rec atmost (elements:'a list) (n: int) : 'a fullcombination list =
if n >= 0 then (exact elements n)@(atmost elements (n-1))
  else [];;

(*`atleast`: finds every m-elements combination with m going from n to the
  number of elemts in `elements` and returns a list of `fullcombination`.
  Note: `fullcombination` is a structure (combination,others) such as
  - `combination`: list of `n` elements defining a combination of n or less elmts
  - `others` is a list of the other elements not picked for this combination*)
let rec atleast (elements:'a list) (n: int) : 'a fullcombination list =
  if n <= (List.length elements) then
    (exact elements n)@(atleast elements (n+1))
  else [];;
