open Solidity_common
open Solidity_ast
open Solidity_checker_TYPES
open Solidity_visitor
open Solidity_postcheck_utils
open Solidity_exceptions

let checkProgram (p : program) : unit =
  let _env = List.fold_left checkModule empty_project_env p.program_modules in
  ()
