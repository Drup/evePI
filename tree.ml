open Eliom_lib
open Eliom_content
open Eliom_content.Html5.D

open Utility

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
  | Leaf node -> li ~a:[a_class ["tree"]] (f node)
  | Node (node, trees) ->
        li ~a:[a_class ["tree"]] 
          (f node @
           [ul ~a:[a_class ["tree"]] (List.map (internal_print f) trees)]
          )

let print f t =
    div ~a:[a_class ["tree"]] 
      [ul ~a:[a_class ["tree"]] [internal_print f t]]

let printf f tl = 
    div ~a:[a_class ["tree"]] 
      [ul ~a:[a_class ["tree"]] (List.map (internal_print f) tl)]

let print_plain =
  let f name = [div ~a:[a_class ["tree"]]  [pcdata name]] in
  print f

let printf_plain = 
  let f name = [div ~a:[a_class ["tree"]]  [pcdata name]] in
  printf f

module Lwt = 
struct 

  let rec map f = function
    | Leaf x -> lwt x = f x in Lwt.return (Leaf x)
    | Node (x,l) ->
        lwt x = f x in
        lwt l = Lwt_list.map_s (map f) l in
        Lwt.return (Node (x,l))

  let mapf f = Lwt_list.map_s (map f)

  let rec internal_print f = function
    | Leaf node -> lwt node = f node in 
        Lwt.return (li ~a:[a_class ["tree"]] node)
    | Node (node, trees) ->
        lwt node = f node in
        lwt content = Lwt_list.map_s (internal_print f) trees in
        Lwt.return (
            li ~a:[a_class ["tree"]]  (node @ [ul ~a:[a_class ["tree"]]  content]
            ))

  let print f t =
    lwt t = internal_print f t in 
    Lwt.return (
        div ~a:[a_class ["tree"]] 
          [ul ~a:[a_class ["tree"]] [t]])

  let printf f tl = 
    lwt tl = Lwt_list.map_s (internal_print f) tl in 
    Lwt.return (
        div ~a:[a_class ["tree"]] 
          [ul ~a:[a_class ["tree"]]  tl])

end		


