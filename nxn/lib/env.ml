type vars = Var of { type' : Ast.types; state : Ast.states; shadow : bool }

type file =
  | File of {
      name : string;
      functions : (string * Ast.types) list;
      structs : (string * Ast.types) list;
      enums : (string * Ast.types) list;
      vars : (string * vars) list;
    }

module Add = struct
  module File = struct
    let function_type id type' env =
      match env with
      | File x ->
          File
            {
              name = x.name;
              functions = (id, type') :: x.functions;
              structs = x.structs;
              enums = x.enums;
              vars = x.vars;
            }
    ;;

    let var id var env =
      match env with
      | File x ->
          File
            {
              name = x.name;
              functions = x.functions;
              structs = x.structs;
              enums = x.enums;
              vars = (id, var) :: x.vars;
            }
    ;;
  end
end
