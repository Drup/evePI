open Eliom_lib
open Eliom_content
open Eliom_content.Html5.D

open Utility
open EvePI_db


(** Decorate a tree according to various information 
	Used in the project_page *)

let decorate_node (id,typeid,notes) = 
  lwt name = Sdd.get_name typeid in 
  lwt users = QPlanet.fetch_by_node id in
  let printer = function
      1 -> "1 planet"
    | n -> Printf.sprintf "%i planets" n in
  let format_user (user,planets) = 
    li ~a:[a_title (printer planets)] [pcdata user]
  in
  Lwt.return (
      [ div
          [span ~a:[a_title notes] [pcdata name] ;
           ul (List.map format_user users)
          ]])

let decorate project = 
  lwt roots_id = QProject.get_roots project in
  lwt trees = 
    Tree.make_forest (fun (id,_,_) -> QProject.get_sons id) roots_id in
  Tree.Lwt.printf decorate_node trees
