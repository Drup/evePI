(* This file regroups all general and utility piece of code used in the project *)

(** 'a option related stuff. Extend {! Eliom_lib.Option} *)
module Option = struct
  include Eliom_lib.Option 

  let map_lwt f = function
	| None -> Lwt.return None
	| Some s -> lwt s = f s in Lwt.return (Some s)

  let map_list f = function
	| Some x -> f x
	| None -> []

  let default s = function
	| Some x -> x
	| None -> s

end


(* Stupid piece of code because of fuck*ng monads *)
let silly f = fun a b -> Lwt.return (fun u -> f a b u)


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

let grouped_map f g l =
  List.map 
    (fun (x,y) -> f x, List.map g y)
    l 

let lwt_grouped_map f g l = 
  Lwt_list.map_p 
    (fun (x,y) -> 
       lwt x = f x in
       lwt y = Lwt_list.map_p g y in
       Lwt.return (x,y)
    ) 
    l

(** Hshtbl *)

let hashtbl_find tbl = Eliom_lib.Option.map (Hashtbl.find tbl)
