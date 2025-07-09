module ModalSystem : sig
  type t = S5
  val of_string : string -> t
  val assert_supported : string -> unit
end

module SolveResult : sig
  type t = SAT | UNSAT
end

module EvaluatedAst : sig
  val translate : ModalSystem.t -> Types.AstSet.elt -> Buffer.t * Buffer.t
  val solve : ModalSystem.t -> Types.AstSet.elt -> SolveResult.t * Buffer.t
end