module File = struct
  let function_type id env =
    match env with
    | Env.File x ->
        let type' = List.assoc_opt id x.functions in
        type'
  ;;

  let var id env =
    match env with
    | Env.File x ->
        let var =
          match List.assoc_opt id x.vars with Some v -> Some v | None -> None
        in
        var
  ;;

  let var_type id env =
    match env with
    | Env.File x ->
        let type' =
          match List.assoc_opt id x.vars with
          | Some (Env.Var v) -> Some v.type'
          | None -> None
        in
        type'
  ;;

  let var_state id env =
    match env with
    | Env.File x ->
        let state =
          match List.assoc_opt id x.vars with
          | Some (Env.Var v) -> Some v.state
          | None -> None
        in
        state
  ;;

  let does_var_shadow id env =
    match env with
    | Env.File x -> (
        match List.assoc_opt id x.vars with
        | Some (Env.Var v) -> v.shadow
        | None -> false)
  ;;
end
