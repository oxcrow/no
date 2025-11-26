(* Silence annoying position/location spam when printing AST. *)
type loc = Loc of { lnum : int; cnum : int }
type pos = Pos of { start : loc; end' : loc }

let show_loc _ = ""
let pp_loc _ _ = ()
let show_pos _ = ""
let pp_pos _ _ = ()

type file = File of { entities : entities list; filename : string }
[@@deriving show { with_path = false }]

and entities = NoneEntity
