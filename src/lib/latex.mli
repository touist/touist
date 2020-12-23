(** Transform any AST (at any stage of transformation) to latex.

    Note that the headers are not included; if you want to compile
    the resulting latex text in a real latex document, you would need
    to add (fr example):
    {[
      \documentclass[11pt]{report}
      \usepackage{mathtools}
      \begin{document}
        % thing produced by latex_of_ast
      \end{document}
    ]}

    Some details on the latex code produced:
    - tuple-propositions [p(a,b,c)] turn into p_{a,b,c}
    - variables are displayed in bold font and '$' is removed
    - we use [\mathbf{}] for setting bold font on variables.
      We could use [\bm{}] (which is a more appropriate way
      of using bold-font in the math env as it keeps the 'italic'
      way of displaying math) but [\usepackage{bm}] does not work
      with most tools: MathJax (javascript), jlatexmath (java).
*)

val latex_of_ast :
  ?matrix_instead_of_substack:bool -> full:bool -> Types.AstSet.elt -> string
(** [latex_of_ast] turns an AST into latex. Two latex variants are targeted:
    - for light latex processors (mathjax, jlatexmath), you should use
      [~full:false]
    - for fully-featured latex processors, you can use [~full:true]. *)

(** {2 Utility} *)

val ast_fun : (bool -> Types.Ast.t -> bool) -> bool -> Types.Ast.t -> bool
(** [ast_fun] will apply f on all *formula*-related elements of the AST where
    cond is true. The tranversal order should not be considered.
    Whenever a non-formula is given, acc will be immediatly returned. *)

val contains_newline : Types.Ast.t -> bool

val is_binary_op : Types.Ast.t -> bool

val contains_binary_op : Types.Ast.t -> bool
