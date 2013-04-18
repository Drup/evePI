open Eliom_lib
open Eliom_content

type 'a tree = Node of 'a * ('a tree list) | Leaf of 'a

let rec make get_sons t =
  lwt l = get_sons t in  
  match l with
  | [] -> Lwt.return (Leaf t)
  | _ -> 
	  lwt trees = Lwt_list.map_s (make get_sons) l in 
	  Lwt.return (Node (t, trees))

let rec map f = function
	 Leaf x -> Leaf (f x)
  | Node (x,l) -> Node(f x, List.map (map f) l)

let rec lwt_map f = function
	 Leaf x -> lwt x = f x in Lwt.return (Leaf x)
  | Node (x,l) ->
	  lwt x = f x in
	  lwt l = Lwt_list.map_s (lwt_map f) l in
	  Lwt.return (Node (x,l))

let print_as_list f t =
  let rec aux = function
	 | Leaf node -> Html5.F.li (f node)
	 | Node (node, trees) ->
		 Html5.F.(
		  li (
				f node @
				[ul (List.map aux trees)]
		))
  in Html5.F.(div ~a:[a_class ["tree"]] [ul [aux t]])
					
let print_as_plain_list t =
  let f name = Html5.F.([a ~service:Eliom_service.void_coservice' [pcdata name] ()]) in
  print_as_list f t
