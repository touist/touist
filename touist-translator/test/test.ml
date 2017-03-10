open OUnit2;;

(* To check that the error has occured curreclty, we only check
   that the place where the error was found is the right one.  *)
let test_raise (parse:(string->unit)) (during:Msg.during) ?(nth_msg=0) (loc_expected:string) text =
  try let _= parse text in
    (Printf.fprintf stdout "%s" "OKç";
    raise (OUnit2.assert_failure (
      "this test should have raised Eval.Error exception with location '"^loc_expected^"'")))
  with Msg.Fatal ->
      match List.nth !Msg.messages nth_msg with
      | (Msg.Error,d,msg,loc) when d==during ->
          OUnit2.assert_equal
          ~msg:("the 'line:column' of expected and actual exception Eval.Error are different; actual error was:\n"^msg)
          ~printer:(fun loc -> Printf.sprintf "'%s'" loc)
          loc_expected (Msg.string_of_loc loc)
      | _ -> raise (OUnit2.assert_failure ("this test didn't raise an error at location '"^loc_expected^"' as expected"))


let sat text = let _= Parse.parse_sat text |> Eval.eval |> Cnf.ast_to_cnf |> Sat.cnf_to_clauses in ()
let smt text = let _= Parse.parse_smt text |> Eval.eval |> Smt.to_smt2 "QF_IDL" in ()

(* The ending _ is necessary because the testing function
   must accept the 'context' thing. *)
let test_sat text _ = sat text
let test_smt text _ = smt text
let test_sat_raise during loc text _ = test_raise sat during loc text
let test_smt_raise during loc text _ = test_raise smt during loc text

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

let read_file (filename:string) : string list =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines ;;

let open_stream name = 
  let chan = open_in name 
  in Stream.from (
    fun _ -> try Some (input_line chan) 
             with End_of_file -> None)


let check_solution (sorted_solution:string) (stream:char Stream.t) =
  let rec one_line stream : string =
    try match Stream.next stream with
      | '\n' -> ""
      | c -> (Printf.sprintf "%c" c) ^ one_line stream
    with Stream.Failure -> "" 
  in 
  let lines_from_stream (stream:char Stream.t) : string list =
    let rec multiple_lines stream = match one_line stream with
    | "" -> []
    | line -> line::(multiple_lines stream)
    in multiple_lines stream
  in
  let rec rm_unwanted_lines (l:string list) : string list = match l with
    | [] -> []
    | x::xs -> if Str.string_match (Str.regexp "^=") x 0
               then rm_unwanted_lines xs
               else x::(rm_unwanted_lines xs)
  in
  let expected = read_file sorted_solution in
  let actual = rm_unwanted_lines (List.sort (compare) (lines_from_stream stream)) in
  let rec check expected actual = match expected,actual with
    | [],[] -> ()
    | [],_ | _,[] -> OUnit2.assert_failure "not the same number of output"
    | exp::xs, act::ys -> OUnit2.assert_equal ~printer:(fun e -> e) exp act;
        (*Printf.fprintf stdout "expected: %s actual: %s\n" exp act;*) check xs ys
  in check expected actual

(* Name the test cases and group them together *)
let () = 
run_test_tt_main (
"touist">:::[

"samples of code that should be correct with -sat and -smt2">:::[ (* 'c' is the testing context *)
  "lower than">::      (test_sat "let $i=1.0: if $i < 3.0 then a else b end");
  "bigand and >">::    (test_sat "bigand $i in [1..5] when $i > 2: p($i) end");
  "let declaration">:: (test_sat "let $i = 3: p($i-$i*3-1 mod 2 / 1)");
  "bigand">::          (test_sat "bigand $i in [a]: p($i) end");
  "bigor">::           (test_sat "bigor $i in [a,b,c] when $i==a and $i!=d: $i(a) end");
  "bigand imply">::    (test_sat "bigand $i,$j in [1..3],[1..3]:
	                                A($i) and B($i) => not C($j) end");
  "affect before">::   (test_sat "$a = a f($a)");
  "affect after">::    (test_sat "f($a) $a = a");
  "affect between">::  (test_sat "$a = a f($a,$b) $b = b");
  
  "var-tuple is prop">::(test_sat "$a=p p($a)");
];

"samples of code that should raise errors in [Eval.eval]">:::[ (* 'c' is the testing context *)
  "undefined var">::         (test_sat_raise Msg.Eval "1:4" "   $a");
  "bigand: too many vars">::(test_sat_raise Msg.Eval "1:8" "bigand $i,$j in [1]: p end");
  "bigand: too many sets">::(test_sat_raise Msg.Eval "1:8" "bigand $i in [1],[2]: p end");
  "bigor: too many vars">::(test_sat_raise Msg.Eval "1:7" "bigor $i,$j in [1]: p end");
  "bigor: too many sets">::(test_sat_raise Msg.Eval "1:7" "bigor $i in [1],[2]: p end");
  "condition is bool">::(test_sat_raise Msg.Eval "1:23" "bigand $i in [1] when a: p end");
  (*"bigand var is not tuple">::(test_sat_raise "1:23:" "bigand $i(p) in [1]: p end");*)
];

"samples of code that should be correct with -smt2">:::[ (* 'c' is the testing context *)
  "lower than">::      (test_smt "let $i=1.0: if $i < 3.0 then a else b end");
  "bigand and >">::    (test_smt "bigand $i in [1..5] when $i > 2: p($i) end");
  "let declaration">:: (test_smt "let $i = 3: p($i-$i*3-1 mod 2 / 1)");
  "bigand">::          (test_smt "bigand $i in [a]: p($i) end");
  "bigor">::           (test_smt "bigor $i in [a,b,c] when $i==a and $i!=d: $i(a) end");
  "affect before">::   (test_smt "$a = a f($a)");
  "affect after">::    (test_smt "f($a) $a = a");
  "affect between">::  (test_smt "$a = a f($a,$b) $b = b");
  
  "affect between">::  (test_smt "a");
  "affect between">::  (test_smt "a > 3");
];
"real-size tests">:::[
  "sodoku">:: (fun ctx -> 
      OUnit2.skip_if (Sys.os_type = "Win32") "won't work on windows (unix-only??)";
      OUnit2.assert_command ~use_stderr:false ~ctxt:ctx
      ~foutput:(check_solution "real-size-tests/sudoku_solution.txt")
      "../touistc.byte" ["--solve";"-sat";"real-size-tests/sudoku.touistl"]);
];

])

;;