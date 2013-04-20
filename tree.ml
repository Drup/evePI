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

let make_forest get_sons = Lwt_list.map_s (make get_sons)

let rec map f = function
	 Leaf x -> Leaf (f x)
  | Node (x,l) -> Node(f x, List.map (map f) l)

let mapf f = List.map (map f)			

let rec internal_print f = function
  | Leaf node -> Html5.F.(li ~a:[a_class ["tree"]] (f node))
  | Node (node, trees) ->
	  Html5.F.(
		li ~a:[a_class ["tree"]] 
		  (f node @
			  [ul ~a:[a_class ["tree"]] (List.map (internal_print f) trees)]
	 ))

let print f t =
  Html5.F.(
	 div ~a:[a_class ["tree"]] 
		  [ul ~a:[a_class ["tree"]] [internal_print f t]])

let printf f tl = 
  Html5.F.(
	 div ~a:[a_class ["tree"]] 
		  [ul ~a:[a_class ["tree"]] (List.map (internal_print f) tl)])
					
let print_plain =
  let f name = Html5.F.([div ~a:[a_class ["tree"]]  [pcdata name]]) in
  print f

let printf_plain = 
  let f name = Html5.F.([div ~a:[a_class ["tree"]]  [pcdata name]]) in
  printf f

module Lwt = 
struct 
	 
let rec map f = function
	 Leaf x -> lwt x = f x in Lwt.return (Leaf x)
  | Node (x,l) ->
	  lwt x = f x in
	  lwt l = Lwt_list.map_s (map f) l in
	  Lwt.return (Node (x,l))
						  
let mapf f = Lwt_list.map_s (map f)
											
let rec internal_print f = function
  | Leaf node -> lwt node = f node in 
					  Lwt.return (Html5.F.(li ~a:[a_class ["tree"]] node))
  | Node (node, trees) ->
	  lwt node = f node in
	  lwt content = Lwt_list.map_s (internal_print f) trees in
	  Lwt.return Html5.F.(
		 li ~a:[a_class ["tree"]]  (node @ [ul ~a:[a_class ["tree"]]  content]
	  ))
	
let print f t =
  lwt t = internal_print f t in 
  Lwt.return Html5.F.(
	 div ~a:[a_class ["tree"]] 
		  [ul ~a:[a_class ["tree"]] [t]])

let printf f tl = 
  lwt tl = Lwt_list.map_s (internal_print f) tl in 
  Lwt.return Html5.F.(
	 div ~a:[a_class ["tree"]] 
		  [ul ~a:[a_class ["tree"]]  tl])

end		
