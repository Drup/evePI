(* This file regroups all general and utility piece of code used in the project *)

(** Opt related *)

let opt_map f = function
	 None -> None 
  | Some s -> Some (f s)

let opt_string = function 
	 Some s -> s
  | None -> ""

(** List related *)

(* Group an 'a * 'b list according to the 'a field. 
THE LIST NEED TO BE ORDERED ! *)
let list_grouping l = 
  let aux l (a,b) = match l with 
		[] -> [(a,[b])] 
	 | (x,y)::t when x = a -> (a,b::y)::t
	 | _ -> (a,[b])::l
  in List.fold_left aux [] l

(** Hshtbl *)

let hashtbl_find tbl = function
  | Some id -> Some (Hashtbl.find tbl id)
  | None -> None 
