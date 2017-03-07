open OUnit2;;

let parse_and_eval expr : unit =
    let _ = Parse.parse_sat expr
            |> Eval.eval in ()


let test1 _ = parse_and_eval
    "bigand $i in [1..5] when $i < 3.0: p($i) end";;
let test2 _ = parse_and_eval
    "bigand $i in [1..5] when $ia > 3.0: p($i) end";;
let test3 _ = parse_and_eval
    "let $i = [3]: p($i-$i*3-1)";;
let test4 _ = parse_and_eval
    "bigand $i in a end";;

(* Name the test cases and group them together *)
let suite =
"suite">:::
    ["lower than">:: test1;
     "greater than">:: test2;
     "let declaration">:: test3;
     "bigand">:: test4]
;;

let () =
  run_test_tt_main suite
;;
