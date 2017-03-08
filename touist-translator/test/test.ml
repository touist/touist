open OUnit2;;

(* The ending _ is necessary because the testing function
   must accept the 'context' thing. *)
let eval text _ =
    let _ = Parse.parse_sat text |> Eval.eval in ()

(* To check that the error has occured curreclty, we only check
   that the place where the error was found is the right one.  *)
let assert_eval_exception (loc_expected:string) text _ =
  let print_loc loc = Printf.sprintf "'%s'" loc in
  let exception_is_correct =
    try let _= Parse.parse_sat text |> Eval.eval in false
    with Eval.Error (_,loc) -> begin
      OUnit2.assert_equal 
        ~msg:"line and column of expected Eval.Error are different"
        ~printer:print_loc loc_expected (Parse.string_of_loc loc);
        true
      end
  in match exception_is_correct with
  | true -> ()
  | false -> raise (OUnit2.assert_failure ("should have raised Eval.Error exception with location "^loc_expected))


(*  A standard test in oUnit should first define a function 
        let test1 context : unit = OUnit2.assert_bool true
    and then you add it to the suite:
        let suite = "suite">:::["test1">::test1;"test2">::test2]
    but instead, I chose to put these functions directly inside the
    list.
    Example of test checking that the exception is raised:
      fun c -> OUnit2.assert_raises (Eval.Error 
      ("incorrect types with '<', which expects a float or int.\n"
       "The content of the variable '$i' has type 'int':\n"
       "    1\n"
       "The other operand is of type 'float':\n    3.",loc))
      (fun _ -> eval "let $i=1: if $i < 3.0 then a else b end"));
*)
let loc = (Lexing.dummy_pos,Lexing.dummy_pos)

(* Name the test cases and group them together *)
let () = 
run_test_tt_main (
"samples of code that should be correct">:::[ (* 'c' is the testing context *)
  "lower than">::      (eval "let $i=1.0: if $i < 3.0 then a else b end");
  "bigand and >">::    (eval "bigand $i in [1..5] when $i > 2: p($i) end");
  "let declaration">:: (eval "let $i = 3: p($i-$i*3-1 mod 2 / 1)");
  "bigand">::          (eval "bigand $i in [a]: p($i) end");
  "bigor">::           (eval "bigor $i in [a,b,c] when $i==a and $i!=d: $i(a) end");
  "affect before">::   (eval "$a = a f($a)");
  "affect after">::    (eval "f($a) $a = a");
  "affect between">::  (eval "$a = a f($a,$b) $b = b");
]); 
run_test_tt_main (
"samples of code that should raise errors in 'eval'">:::[ (* 'c' is the testing context *)
  "not empty">::       (assert_eval_exception "2:2:" "\n $a");
  "bigand: too many vars">::(assert_eval_exception "1:8:" "bigand $i,$j in [1]: p end");
  "bigand: too many sets">::(assert_eval_exception "1:8:" "bigand $i in [1],[2]: p end");
  "bigor: too many vars">::(assert_eval_exception "1:7:" "bigor $i,$j in [1]: p end");
  "bigor: too many sets">::(assert_eval_exception "1:7:" "bigor $i in [1],[2]: p end");
])
;;
