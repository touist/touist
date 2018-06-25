open OUnit2;;

(* We redirect every warnings to /dev/null *)
Touist.Err.discard_warnings := true;;

(* To check that the error has occured curreclty, we only check
   that the place where the error was found is the right one.  *)

let is_msg typ during loc_str msg =
  match msg with
  | t,d,_,Some l when String.compare (Touist.Err.string_of_loc l) loc_str = 0
        && t == typ
        && d == during -> true
  | t,d,_,None when loc_str = ""
        && t == typ
        && d == during -> true
  | _ -> false

let test_raise (parse:(string->unit)) (during:Touist.Err.during) typ nth_msg (loc_expected:string) text =
  try parse text;
    if typ == Touist.Err.Error then
      OUnit2.assert_failure ("this test didn't raise an error at location '"^loc_expected^"' as expected")
  with Touist.Err.TouistFatal msg ->
    match msg |> is_msg typ during loc_expected with
    | true -> () (* OK *)
    | false -> OUnit2.assert_failure ("this test didn't give a message at location '"^loc_expected^"' as expected. Instead, got:\n"^Touist.Err.string_of_msg msg)

let sat text = let _ = Touist.Parse.parse_sat text |> Touist.Eval.eval |> Touist.Cnf.ast_to_cnf |> Touist.SatSolve.minisat_clauses_of_cnf in ()
let smt logic text = let _ = Touist.Parse.parse_smt text |> Touist.Eval.eval ~smt:true |> Touist.Smt.to_smt2 logic in ()
let qbf text = let _ = Touist.Parse.parse_qbf text |> Touist.Eval.eval ~smt:true |> Touist.Qbf.prenex in ()

(* The ending _ is necessary because the testing function
   must accept the 'context' thing. *)
let test_sat text _ =
  try sat text
  with Touist.Err.TouistFatal msg -> OUnit2.assert_failure
    ("this test shouldn't have raised a TouistFatal exception. Here is the exception:\n"^
      Touist.Err.string_of_msg msg)

let test_smt ?(logic="QF_IDL") text _ =
  try (smt logic) text
  with Touist.Err.TouistFatal msg -> OUnit2.assert_failure
    ("this test shouldn't have raised a TouistFatal exception. Here is the exception:\n"^
      Touist.Err.string_of_msg msg)

let test_qbf text _ =
  try qbf text
  with Touist.Err.TouistFatal msg -> OUnit2.assert_failure
    ("this test shouldn't have raised a TouistFatal exception. Here is the exception:\n"^
      Touist.Err.string_of_msg msg)

let test_sat_raise ?(during=Touist.Err.Eval) ?(typ=Touist.Err.Error) ?(nth=0) loc text _ = test_raise sat during typ nth loc text
let test_smt_raise ?(during=Touist.Err.Eval) ?(typ=Touist.Err.Error) ?(nth=0) ?(logic="QF_IDL") loc text _ = test_raise (smt logic) during typ nth loc text
let test_qbf_raise ?(during=Touist.Err.Eval) ?(typ=Touist.Err.Error) ?(nth=0) ?(logic="QF_IDL") loc text _ = test_raise qbf during typ nth loc text

let sat_models_are text expected _ =
  OUnit2.assert_equal ~printer:(fun s -> s)
    expected
    (let ast = Touist.Parse.parse_sat text |> Touist.Eval.eval in let cl,tbl = Touist.Cnf.ast_to_cnf ast |> Touist.SatSolve.minisat_clauses_of_cnf in
      let models_str = ref [] in
        let _ = Touist.SatSolve.solve_clauses ~print:(fun tbl m _ -> models_str := (Touist.SatSolve.Model.pprint ~sep:" " tbl m)::!models_str) (cl,tbl)
          in List.fold_left (fun acc s -> match acc with "" -> s | _ -> s^" | "^acc) "" !models_str)

          (* !models_str
        |> List.sort (fun s1 s2 -> let open String in
          compare (sub s1 2 (length s1)) (sub s2 2 (length s2)))
        |> List.fold_left (fun acc s -> match acc with "" -> s | _ -> s^" | "^acc) "") *)


(* Tests if the given [text] is translated into the [expected] expanded
   text. *)
let sat_expands_to text expected _ =
  OUnit2.assert_equal ~printer:(fun s -> s)
    expected (let ast = Touist.Parse.parse_sat text |> Touist.Eval.eval in Touist.Pprint.string_of_ast ast)

let qbf_expands_to text expected _ =
  OUnit2.assert_equal ~printer:(fun s -> s)
    expected (let ast = Touist.Parse.parse_qbf text |> Touist.Eval.eval |> Touist.Qbf.prenex in Touist.Pprint.string_of_ast ast)

(*  A standard test in oUnit should first define a function
        let test1 context : unit = OUnit2.assert_bool true
    and then you add it to the suite:
        let suite = "suite">:::["test1">::test1;"test2">::test2]
    but instead, I chose to put these functions directly inside the
    list.
    Example of test checking that the exception is raised:
      fun c -> OUnit2.assert_raises (Touist.Eval.Error
      ("incorrect types with '<', which expects a float or int.\n"
       "The content of the variable '$i' has type 'int':\n"
       "    1\n"
       "The other operand is of type 'float':\n    3.",loc))
      (fun _ -> eval "let $i=1: if $i < 3.0 then a else b end"));
*)

and read_file (filename:string) : string list =
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
    | x::xs -> if Re.Str.string_match (Re.Str.regexp "^=") x 0
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

"numerical expressions">:::[ (* 'c' is the testing context *)
  "1 > 10 should be false">::(sat_expands_to "t(1 > 10)" "t(false)");
  "1 < 10 should be true">::(sat_expands_to "t(1 < 10)" "t(true)");
  "1.0 > 10.0 should be false">::(sat_expands_to "t(1.0 > 10.0)" "t(false)");
  "1.0 < 10.0 should be true">::(sat_expands_to "t(1.0 < 10.0)" "t(true)");
  "1 == 1.0 should raise error">::(test_sat_raise "1:3" "t(1==1.0)");
  "1.0 == 1 should raise error">::(test_sat_raise "1:3" "t(1.0==1)");
  "1 == 1 should be true">::(sat_expands_to "t(1==1)" "t(true)");
];
"unary operators">:::[
  "abs()">::(sat_expands_to "p(abs(-1))" "p(1)");
  "card()">::(sat_expands_to "p(card([1..4]))" "p(4)");
  "empty()">::(sat_expands_to "p(empty([]))" "p(true)");
  "empty()">::(sat_expands_to "p(empty([1]))" "p(false)");
];

"binary operators">:::[
  "inter prefix">::(sat_expands_to "p(inter([1],[1,2]))" "p([1])");
  "inter infix">::(sat_expands_to "p([1] inter [1,2])" "p([1])");
  "union prefix">::(sat_expands_to "p(union([1],[1,2]))" "p([1,2])");
  "union infix">::(sat_expands_to "p([1] union [1,2])" "p([1,2])");
  "diff prefix">::(sat_expands_to "p(diff([1,2],[2]))" "p([1])");
  "diff infix">::(sat_expands_to "p([1,2] diff [2])" "p([1])");
  "subset prefix">::(sat_expands_to "p(subset([1],[1,2]))" "p(true)");
  "subset infix">::(sat_expands_to "p([1] subset [1,2])" "p(true)");
];

"exact, atleast and atmost">:::[
  "exact(5,[]) should be false">::(sat_expands_to "exact(5,[])" "Bot");
  "exact(0,[]) should be true">::(sat_expands_to "exact(0,[])" "Top");
  "atmost(0,[]) should be true">::(sat_expands_to "atmost(0,[])" "Top");
  "atmost(5,[]) should be true">::(sat_expands_to "atmost(5,[])" "Top");
  "atleast(0,[]) should be true">::(sat_expands_to "atleast(0,[])" "Top");
  "atleast(5,[]) should be false">::(sat_expands_to "atleast(5,[])" "Bot");
  "normal cases">:::[
  "exact(0,[a,b]) should return 'not a and not b'">::(sat_expands_to "exact(0,[a,b])" "(not a and not b)");
  "exact(1,[a,b,c]) should give 3 models">::(sat_models_are "exact(1,[a,b,c])" "0 b 0 a 1 c | 0 b 1 a 0 c | 1 b 0 a 0 c");
  "exact(3,[a,b,c]) should give 1 model">::(sat_models_are "exact(3,[a,b,c])" "1 c 1 b 1 a");
  "'atmost(2,[a,b,c]) a' should give 3 models">::(sat_models_are "atmost(2,[a,b,c]) a" "1 a 0 b 0 c | 1 a 0 b 1 c | 1 a 1 b 0 c");
  "'atmost(2,[a,b,c]) a b' should give 1 model">::(sat_models_are "atmost(2,[a,b,c]) a b" "1 b 1 a 0 c");
  "'atleast(2,[a,b,c])' should give 4 model">::(sat_models_are "atleast(2,[a,b,c])" "1 c 1 b 0 a | 1 c 1 b 1 a | 0 c 1 b 1 a | 1 c 0 b 1 a");
  "'atleast(2,[a,b,c]) a' should give 3 model">::(sat_models_are "atleast(2,[a,b,c]) a" "1 a 1 b 1 c | 1 a 1 b 0 c | 1 a 0 b 1 c");
  "'atleast(2,[a,b,c]) a b' should give 2 model">::(sat_models_are "atleast(2,[a,b,c]) a b" "1 b 1 a 1 c | 1 b 1 a 0 c");
  ];
"bigand and bigor">:::[
  "empty cases">:::[
  "bigand with empty sets should return Top">::(sat_expands_to "bigand $i in []: p($i) end" "Top");
  "bigand with always false 'when' should return Top">::(sat_expands_to "bigand $i in [1] when false: p($i) end" "Top");
  "bigor with empty sets should return Bot">::(sat_expands_to "bigor $i in []: p($i) end" "Bot");
  "bigor with always false 'when' should return Bot">::(sat_expands_to "bigor $i in [1] when false: p($i) end" "Bot");
  ];
  "bigand and >">::    (test_sat "bigand $i in [1..5] when $i > 2: p($i) end");
  "let declaration">:: (test_sat "let $i = 3: p($i-$i*3-1 mod 2 / 1)");
  "let multiple declaration">:: (sat_expands_to "let $i,$j = 3,4: p($i,$j)" "p(3,4)");
  "bigand">::          (test_sat "bigand $i in [a]: p($i) end");
  "bigor">::           (test_sat "bigor $i in [a,b,c] when $i==a and $i!=d: $i(a) end");
  "bigand imply">::    (test_sat "bigand $i,$j in [1..3],[1..3]:
	                                A($i) and B($i) => not C($j) end");
  "affect before">::   (test_sat "$a = a f($a)");
  "affect after">::    (test_sat "f($a) $a = a");
  "affect between">::  (test_sat "$a = a f($a,$b) $b = b");
  "var-tuple is prop">::(test_sat "$a=p p($a)");
  ];
];
"set of sets">:::[
  "simple set of set">::(sat_expands_to "f([[1,2],[3,4]])" "f([[1,2],[3,4]])");
  "should not mix types">::(fun _ -> OUnit2.skip_if true "should not mix types"; sat_expands_to "f([[1,2],[a,b]])" "" ());
];
"powerset">:::[
  "powerset should always return a set containing the empty set">::(sat_expands_to "f(powerset([1]))" "f([[],[1]])");
  "powerset simple test">::(sat_expands_to "f(powerset([a,b]))" "f([[],[a],[a,b],[b]])")
];
"set builder/list comprehension">:::[
  "should expand to a set of 4 elmts">::(sat_expands_to "f([p($i,$j) for $i,$j in [1,2],[a,b]])" "f([p(1,a),p(1,b),p(2,a),p(2,b)])");
  "can have a 'when'">::(sat_expands_to "f([p($i) for $i in [1,2,3] when $i>1])" "f([p(2),p(3)])");
];

"samples of code that should raise errors in {!Touist.Eval.eval}">:::[ (* 'c' is the testing context *)
  "undefined var">::         (test_sat_raise "1:4" "   $a");
  "bigand: too many vars">::(test_sat_raise "1:8" "bigand $i,$j in [1]: p end");
  "bigand: too many sets">::(test_sat_raise "1:8" "bigand $i in [1],[2]: p end");
  "bigor: too many vars">::(test_sat_raise "1:7" "bigor $i,$j in [1]: p end");
  "bigor: too many sets">::(test_sat_raise "1:7" "bigor $i in [1],[2]: p end");
  "condition is bool">::(test_sat_raise "1:23" "bigand $i in [1] when a: p end");
  (*"bigand var is not tuple">::(test_sat_raise "1:23:" "bigand $i(p) in [1]: p end");*)
];

"test of the p([a,b,c]) construct">:::[ (* 'c' is the testing context *)
  "p([a,b]) in a formula should stay p([a,b])">::(sat_expands_to "p([a,b])" "p([a,b])");
  "p([a,b]) in an expr should expand to [p(a),p(b)]">::(sat_expands_to "t(p([a,b]))" "t([p(a),p(b)])");
  "p(a) in an expr shouldn't expand to a set">::(sat_expands_to "t(p(a))" "t(p(a))");
  "p([]) in an expr should return p">::(sat_expands_to "t(p([]))" "t(p)");
  "p([],a) in an expr should return p(a)">::(sat_expands_to "t(p([],a))" "t(p(a))");
  "p(a,[]) in an expr should return p(a)">::(sat_expands_to "t(p(a,[]))" "t(p(a))");
  "p(1,[a]) in an expr should return [p(1,a)]">::(sat_expands_to "t(p(1,[a]))" "t([p(1,a)])");
  "the p([a,b,c]) syntax">:: (fun ctx ->
        OUnit2.skip_if (Sys.os_type = "Win32") "won't work on windows (unix-only??)";
        OUnit2.assert_command ~use_stderr:false ~ctxt:ctx
        ~foutput:(check_solution "sat/unittest_setgen_solution.txt")
        "jbuilder" ["exec";"--";"touist";"--solve";"--sat";"sat/unittest_setgen.touist"]);
];

"samples of code that should be correct with --smt">:::[ (* 'c' is the testing context *)
  "">::(test_smt "a > 1");
  "">::(test_smt "a < 1");
  "">::(test_smt "a == 3");
  "">::(test_smt "a != 3");
  "for now, one of the two terms must be a float or int">::(test_smt "(a+1) > 3");
  "takuzu4x4.touist">:: (test_smt (Touist.Parse.string_of_file "smt/takuzu4x4.touist"))
];

"QBF testing">:::[
  "samples of code that should be correct with --qbf">:::[ (* 'c' is the testing context *)
    "">::(test_qbf "exists x: x");
    "">::(test_qbf "forall x: x");
    "">::(test_qbf "exists a,b: a and b");
    "">::(test_qbf "forall a,b: a and b");
    "">::(test_qbf "forall a: exists b: a and b");
    "allumettes2.touist">:: (test_qbf (Touist.Parse.string_of_file "qbf/allumettes2.touist"));
  ];
  "samples that shouldn't be correct with --qbf">:::[
    "quantified var must be a prop">::(test_qbf_raise "1:13" "$x=1 exists $x: x");
  ];
  "test of exists ... for ... in ...:">:::[
    "">::(qbf_expands_to "exists p($i) for $i in [1..2]: p(1) and p(2)" "exists p(2): exists p(1): (p(1) and p(2))");
  ];
  "prenex tests">:::[ (* 'c' is the testing context *)
    "prenex renaming pbm: cannot rename inner 'a', is it bounded to the outer \
    exists or the inner exists?">::(test_qbf_raise ~during:Touist.Err.Prenex "" "exists a: (a and exists a: a)");
    "prenex renaming 2">::(qbf_expands_to "(exists a: a) and (exists a: a)" "exists a: exists a_1: (a and a_1)");
    "prenex renaming 2">::(qbf_expands_to "(exists a: a) and (a and exists a: a)" "exists a: exists a_1: exists a_2: (a and (a_2 and a_1))");
  ];
];

"real-size tests">:::[
  "with --sat --solve">:::[
  "sat/sodoku.touist (SAT solver)">:: (fun ctx ->
      OUnit2.skip_if (Sys.os_type = "Win32") "won't work on windows (unix-only??)";
      OUnit2.assert_command ~use_stderr:false ~ctxt:ctx
      ~foutput:(check_solution "sat/sudoku_solution.txt")
      "jbuilder" ["exec";"--";"touist";"--solve";"--sat";"sat/sudoku.touist"]);
  "sat/minisat_clause_add_unsat.touist, should be unsat">:: (fun ctx ->
      OUnit2.skip_if (Sys.os_type = "Win32") "won't work on windows (unix-only??)";
      OUnit2.assert_command
        ~exit_code:(Unix.WEXITED 8)
        ~use_stderr:true ~ctxt:ctx
      "jbuilder" ["exec";"--";"touist";"--solve";"--sat";"sat/minisat_clause_add_unsat.touist"]);
  ];
  "with --smt --solve">:::[
  "sat/sodoku.touist (using SMT QF_BV solver)">:: (fun ctx ->
        OUnit2.skip_if (Sys.os_type = "Win32") "won't work on windows (unix-only??)";
        OUnit2.skip_if (not Touist_yices2.SmtSolve.enabled) "touist built without yices2";
        OUnit2.assert_command ~use_stderr:false ~ctxt:ctx
        ~foutput:(check_solution "sat/sudoku_solution.txt")
        "jbuilder" ["exec";"--";"touist";"--solve";"--smt";"QF_BV";"sat/sudoku.touist"]);
  "smt/takuzu4x4.touist">:: (fun ctx ->
      OUnit2.skip_if (Sys.os_type = "Win32") "won't work on windows (unix-only??)";
      OUnit2.skip_if (not Touist_yices2.SmtSolve.enabled) "touist built without yices2";
      OUnit2.assert_command ~use_stderr:false ~ctxt:ctx
      ~foutput:(check_solution "smt/takuzu4x4_solution.txt")
      "jbuilder" ["exec";"--";"touist";"--solve";"--smt";"QF_IDL";"smt/takuzu4x4.touist"]);
  ];
  "with --qbf --solve">:::[
  "sat/sodoku.touist (using QBF solver)">:: (fun ctx ->
        OUnit2.skip_if (Sys.os_type = "Win32") "won't work on windows (unix-only??)";
        OUnit2.skip_if (not Touist_qbf.QbfSolve.enabled) "touist built without qbf";
        OUnit2.assert_command ~use_stderr:false ~ctxt:ctx
        ~foutput:(check_solution "sat/sudoku_solution.txt")
        "jbuilder" ["exec";"--";"touist";"--solve";"--qbf";"sat/sudoku.touist"]);
  "qbf/allumettes2.touist">:: (fun ctx ->
      OUnit2.skip_if (Sys.os_type = "Win32") "won't work on windows (unix-only??)";
      OUnit2.skip_if (not Touist_qbf.QbfSolve.enabled) "touist built without qbf";
      OUnit2.assert_command ~use_stderr:false ~ctxt:ctx
      ~foutput:(check_solution "qbf/allumettes2.solution")
      "jbuilder" ["exec";"--";"touist";"--solve";"--qbf";"qbf/allumettes2.touist"]);
  ]
];

"formula-vars tests">:::[
  "$x:=a and b expansion">:: (sat_expands_to {|$x="a and b"   $x|} "(a and b)");
  {|$F=["a or b", "a or not b"] expansion|}>:: (sat_expands_to {|$F=["a or b", "a or not b"]   bigand $f in $F: $f end|}
  "((a or not b) and (a or b))");
  {|$F=[["a or b", "a and not b"],["a and c"]] expansion|}>:: (sat_expands_to {|$F=[["a or b", "a and not b"],["a and c"]]
  bigand $f in $F: bigor $i in $f: $i end end|}
  "((a and c) and ((a and not b) or (a or b)))");
];

])

;;
