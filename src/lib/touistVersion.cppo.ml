(** Version information for the TouIST library.

    This file is computed by cppo (see Makefile).
    - {!version} is the OPAM version (as seen in the _oasis file)
    - {!has_git_tag} tells if the library has been built inside a git repo;
      if it has, then {!git_tag} contains the latest git tag using
      [git describe --tags] *)

let version = VERSION
let has_git_tag = HAS_GIT_TAG
let git_tag = GIT_TAG

let has_yices2 = HAS_YICES2
let has_qbf = HAS_QBF
let has_depqbf = HAS_DEPQBF
