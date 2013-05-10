(* This file regroups all general and utility piece of code used in the project *)

(** Opt related *)

let opt_map f = function
  | None -> None 
  | Some s -> Some (f s)

let lwt_opt_map f = function
  | None -> Lwt.return None
  | Some s -> lwt s = f s in Lwt.return (Some s)

let opt_iter f = function
  | None -> ()
  | Some s -> f s

let opt_string ?(def="")= function 
  | Some s -> s
  | None -> def

let opt_unnamed = function
  | Some s -> s
  | None -> "Unnamed"

let opt_map_list f = function
  | Some x -> f x
  | None -> []

let opt_list = function
  | Some x -> x
  | None -> []

(** List related *)

(* Group an 'a * 'b list according to the 'a field. 
THE LIST NEED TO BE ORDERED ! *)
let list_grouping l = 
  let aux l (a,b) = match l with 
		[] -> [(a,[b])] 
	 | (x,y)::t when x = a -> (a,b::y)::t
	 | _ -> (a,[b])::l
  in List.fold_left aux [] l

let list_grouping_sort l = 
  list_grouping (List.sort (fun x y -> compare y x) l)

(** Hshtbl *)

let hashtbl_find tbl = function
  | Some id -> Some (Hashtbl.find tbl id)
  | None -> None 
