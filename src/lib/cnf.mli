(** Process an evaluated AST given by {!Eval.eval}
    to produce a CNF AST and output DIMACS

    [ast_to_cnf] is the main function.

    {2 Vocabulary}

    {ul
    {- {b Literal}:
      a possibly negated proposition; we denote them as a, b... and
      their type is homogenous to [Prop _] or [Not(Prop _)] or [Top] or [Bottom].
      Exples:
        - [ a        ]                        is a literal,
        - [ not b    ]                        is a literal.

    }{- {b Clause}:
      a disjunction (= separated by "or") of possibly negated literals.
      Example of clause:
        - [ a or not b or c or d   ]          is a clause

    }{- {b Conjunction}:
      literals separated by "and"; example:
        - [ a and b and not and not d    ]    is a conjunction

    }{- {b AST}:
      abstract syntax tree; it is homogenous to {!Types.Ast.t}
      and is a recursive tree representing a formula, using Or, And, Implies...
      Example: the formula (1) has the abstract syntax tree (2):
        - [ (a or b) and not c    ]                  (1) natural language
        - [ And (Or (Prop x, Prop x),Not (Prop x))  ](2) abstract syntax tree

    }{- {b CNF}:
      a Conjunctive Normal Form is an AST that has a special structure with
      is a conjunction of disjunctions of literals. For example:
        - [ (a or not b) and (not c and d)   ]    is a CNF form
        - [ (a and b) or not (c or d)        ]    is not a CNF form

    }}
*)

(** {2 CNF transformation} *)

(** [ast_to_cnf] translates the syntaxic tree made of Or, And, Implies, Equiv...
    Or, And and Not; moreover, it can only be in a conjunction of formulas
    (see a reminder of their definition above).
    For example (instead of And, Or we use "and" and "or" and "not"):
    {v
        (a or not b or c) and (not a or b or d) and (d)
    v}
    The matching abstract syntax tree (ast) is
    {v
        And (Or a,(Cor (Not b),c)), (And (Or (Or (Not a),b),d), d)
    v}
*)
val ast_to_cnf : ?debug_cnf:bool -> Types.Ast.t -> Types.Ast.t


(** {2 Clauses transformation} *)

(** [clauses_of_cnf] translates the cnf ast (Not, And, Or, Prop; no Bot/Top)
    into a CNF formula that takes the form of a list of lists of litterals
    (conjunctions of disjunctions of possibly negated proprositions).
    [neg lit] returns the negation of the litteral (not)
    [fresh ()] returns a newly generated litteral
    Returns:
    - the list of lists of litterals
    - the table literal-to-name
    Note that the total number of literals is exactly equal to the table size;
    this size includes the special propositions beginning with '&' (e.g., '&4').
*)
val clauses_of_cnf :
  ('a -> 'a) ->
  (unit -> 'a) ->
  Types.Ast.t ->
  'a list list * ('a, string) Hashtbl.t * (string, 'a) Hashtbl.t

(** {2 DIMACS output}

    The following functions are for displaying dimacs/qdimacs format.
    Example for the formula
    {v
        rain=>wet and rain and not wet
    v}
    we get the dimacs file:
    {v
        c wet 1                           <-- (optionnal) [print_table]
        c rain 2
        c CNF format file                 <-- by hand
        p cnf 2 3                         <-- by hand (nb_lits, nb_clauses)
        -2 1 0                            <-- [print_clauses]
        -2 2 0
        -2 -1 0
    v}
*)

(** [print_table] prints the correspondance table between literals (= numbers)
    and user-defined proposition names, e.g.,
    {v
        p(1,2) 98
    v}
    where 98 is the literal id number (given automatically) and p(1,2) is the
    name of this proposition.

    NOTE: you can add a prefix to 'p(1,2) 98', e.g. {[
        string_of_table ~prefix:"c " table
    ]}
    in order to have all lines beginning by 'c' (= comment) in order to comply to
    the DIMACS format. *)
val print_table :
  ('a -> int) ->
  out_channel -> ?prefix:string -> ('a, string) Hashtbl.t -> unit

(** [print_clauses] prints one disjunction per line ended by 0:
    {v
       -2 1 0
       -2 2 0
    v}
    IMPORTANT: prints ONLY the clauses. You must print the dimacs/qdimacs
    header yourself, e.g.:
    {v
       p cnf <nb_lits> <nb_clauses>      with <nb_lits> = Hashtbl.length table
                                              <nb_clauses> = List.length clauses
    v} *)
val print_clauses :
  out_channel -> ?prefix:string -> ('a -> string) -> 'a list list -> unit

(** {2 Other functions} *)

(** [is_dummy name] tells (using the [name] of a litteral) is a 'dummy' literal
    that was introduced during cnf conversion; these literals are identified
    by their prefix '&'. *)
val is_dummy : string -> bool