open Core

type file = File of { entities : int list; file : string } [@@deriving show { with_path = false }]
