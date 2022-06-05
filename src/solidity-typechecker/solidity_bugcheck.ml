open Solidity_common
open Solidity_ast
(* open Solidity_checker_TYPES
(* open Solidity_visitor
open Solidity_postcheck_utils *)
open Solidity_exceptions *)


let string_of_ident id =
  Ident.to_string (strip id)


let checkFuncDef (fd : function_definition) = 
  Printf.printf "%s\n" (string_of_ident fd.fun_name)


let rec checkPartList (pl : (contract_part node) list) =
  match pl with
  | [] -> ()
  | cp :: new_pl ->
    let () = 
      (fun
        {contents;annot;_} ->
        match contents,annot with
        | FunctionDefinition funcDef, _ -> 
          checkFuncDef funcDef
        | _ -> ()
      )
      cp
    in 
    checkPartList new_pl


let checkModule (m : module_) =
  let rec checkSourceUnit (ul : (source_unit node) list) = 
    match ul with
    | [] -> ()
    | u :: new_ul ->
      let () = 
          (fun 
            {contents;annot;_} ->
            match contents,annot with
            | ContractDefinition contract, _ ->
              checkPartList contract.contract_parts
            | _ -> ()
          ) 
          u
      in checkSourceUnit new_ul
  in checkSourceUnit m.module_units
let rec checkModuleList (ml: module_ list) = 
  match ml with
  | [] -> ()
  | m :: new_ml -> 
    let () = checkModule m in
      checkModuleList new_ml


let checkProgramBug (p : program) =
  checkModuleList p.program_modules
