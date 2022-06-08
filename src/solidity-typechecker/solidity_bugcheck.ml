open Solidity_common
open Solidity_ast
(* open Solidity_checker_TYPES *)
(* open Solidity_visitor
open Solidity_postcheck_utils *)
open Solidity_exceptions
open Solidity_printer

let string_of_ident id =
  Ident.to_string (strip id)

let print_ident id = 
  Printf.printf "%s;" (string_of_ident id)


type variable_env = ident list

(** TODO: in env1 and env2 we need different add strategy
  in env1 we need all variavles
  in env2 we just consider the writes
  need to split the two functions
*)
let rec add_var_to_env (e : expression) (env : variable_env) : variable_env= 
  
  let new_env = 
    match e.contents with
    | IdentifierExpression id -> 
      [id] @ env
    | AssignExpression (e1, e2) ->
      let temp_env1 = add_var_to_env e1 env
        in add_var_to_env e2 temp_env1
    | AssignBinaryExpression  (e1, _, e2) ->
      let temp_env1 = add_var_to_env e1 env
      in add_var_to_env e2 temp_env1
    | CompareExpression  (e1, _, e2) ->
      let temp_env1 = add_var_to_env e1 env
      in add_var_to_env e2 temp_env1
    | ArrayAccess (e1, e2) -> 
      begin
      let temp_env1 = add_var_to_env e1 env
      
        in match e2 with
          | Some _e2 ->
            add_var_to_env _e2 temp_env1
          | None -> temp_env1
      end
    | TupleExpression ts ->
        let rec visitTs (_ts) (_en) = 
          match _ts with
          | [] -> _en
          | elem :: rest_ts ->
            let temp_env = 
              begin
              match elem with
              | Some e -> (add_var_to_env e _en)
              | None -> _en
              end
            in visitTs rest_ts temp_env
        in visitTs ts env
    | _ -> env
  in new_env

let rec print_env (en : variable_env) = 
  match en with 
  | [] -> Printf.printf "]\n"
  | id :: new_en ->
    let () = Printf.printf "[" in
    let () = print_ident id
    in print_env new_en

let check_reentrancy (env1 : variable_env) (env2 : variable_env) (judge : bool ref) = 
  let () = 
    Printf.printf "["
  in let () =
    print_env env1
  in
  let () = 
    Printf.printf "["
    in
  let () = print_env env2
  in
  if (!judge) then 
    let rec matchVar1 e1 e2 = 
      match e1 with
      | [] -> ()
      | var1 :: rest_env1 ->
        let () = 
          let rec matchVar2 _var1 _env2=
            match _env2 with 
            | [] -> ()
            | var2 :: rest_env2 ->
              if (Ident.equal _var1.contents var2.contents) 
                then raise (ReentrancyRisk "test")
              else
                matchVar2 _var1 rest_env2
          in matchVar2 var1 e2
        in matchVar1 rest_env1 e2
    in matchVar1 env1 env2
        
let rec is_substring pos s1 s2 = 
  let result = 
    let len2 = (String.length s2)
    in let len1 = (String.length s1)
    in if  ((pos + len1) > len2) then false
      else 
      begin
        if (String.equal s1 (String.sub s2 pos len1)) then true
        else is_substring (pos+1) s1 s2
      end
  in result


let is_transfer (e : expression) : bool = 
  (* TODO: A better transfer detect approach*)
  let e_str = string_of_expression e
  in let transfer_str = "transfer"
  in is_substring 0 transfer_str e_str

(* let is_send (e : expression) : bool = 
  (* TODO: A better transfer detect approach*)
  let e_str = string_of_expression e
  in let transfer_str = "send"
  in is_substring 0 transfer_str e_str *)


let checkFuncDef (fd : function_definition) = 
  match fd.fun_body with
  | Some b->
    let after_transfer = ref false
    in
          let rec visit_statements (rsl : block) (_env1_ : variable_env) (_env2_ : variable_env) (_judge_ : bool ref) = 
          let (f_e1, f_e2) = 
            match rsl with
            | [] -> (_env1_, _env2_)
            | _rs :: new_rsl ->
              let (new_env1, new_env2) = 
                    begin
                    let rec visit_one_statement rs env1 env2 judge = 
                      let (new_e1, new_e2) = 
                        match rs.contents with
                        | IfStatement (if_,then_,else_) -> 
                          if !judge then (env1, (add_var_to_env if_ env2))
                          else  
                            begin
                              let (n_e1, n_e2) = 
                                    ((add_var_to_env if_ env1), env2) 
                              in 
                              let (nn_e1, nn_e2) = 
                                visit_one_statement then_ n_e1 n_e2 judge
                              in 
                                match else_ with
                                | Some s ->
                                    visit_one_statement s nn_e1 nn_e2 judge
                                | None -> (nn_e1, nn_e2)
                              end
                                  (* TODO: need to finish below*)
                                  (* add_var_to_env then_ env1
                                  in 
                                  add_var_to_env else_ env1
                                  in () *)
                        | ExpressionStatement e ->
                          if (is_transfer e) then 
                            let () = (judge := true)
                             in (env1, env2)
                          else 
                          (if !judge then (env1, (add_var_to_env e env2))
                            else ((add_var_to_env e env1), env2))
                        | _ -> (env1, env2)
                      in (new_e1, new_e2)
                    in visit_one_statement _rs _env1_ _env2_ _judge_
                    end
              in
              visit_statements new_rsl new_env1 new_env2 _judge_
            in (f_e1, f_e2)
          in 
        let (final_env1, final_env2) = 
            visit_statements b [] [] after_transfer
        in check_reentrancy final_env1 final_env2 after_transfer
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
