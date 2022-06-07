open Solidity_common
open Solidity_ast
(* open Solidity_checker_TYPES
(* open Solidity_visitor
open Solidity_postcheck_utils *)
open Solidity_exceptions *)


let string_of_ident id =
  Ident.to_string (strip id)


let print_all_names (exp : expression) = 
  let () = raise (ReentrancyRisk "test")
  in
  ()

type variable_env = ident list



let checkFuncDef (fd : function_definition) = 
  match fd.fun_body with
  | Some b->
    let empty_variable_env1 = []
    in
      let empty_variable_env2 = []
      in
        let after_transfer = ref false
        in
          let rec visit_statement (rsl : block) (env1 : variable_env) (env2 : variable_env) (judge : bool)= 
            match rsl with
            | [] -> ()
            | rs :: new_rsl ->
              let () = 
                (fun
                  {contents;_} ->
                  match contents with
                  | ExpressionStatement exp -> 
                    print_all_names exp
                  | _ -> ()
                )
              in
              visit_statement new_rsl env1 env2 judge
          in 
          visit_statement b empty_variable_env1 empty_variable_env2  after_transfer
  | None -> ()


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
