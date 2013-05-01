{shared{
open Eliom_lib
open Eliom_content
open Eliom_service
open Eliom_content.Html5
open Eliom_content.Html5.D
open Bootstrap
}}

open Skeleton
open Skeleton.Connected
open Auth
open EvePI_db


let make_link_member_project (project_id,project_name) = 
  a ~service:project_member_coservice [pcdata project_name] project_id

let make_link_project_page project_id project_name = 
  a ~a:(classe "btn") ~service:project_member_coservice [pcdata project_name] project_id



(** Join a project *)

let join_project_service = 
  Eliom_service.post_coservice'
    ~post_params:Eliom_parameter.(int64 "project") ()

let join_project_button project_id =
  button
    ~button_type:`Button
    ~a:[lclasses ["btn"] ; 
        a_onclick 
          {{ fun _ -> 
             ignore (Eliom_client.change_page  
                 ~service:%join_project_service () %project_id) }}]
    [pcdata "Join now !"]

let _ =
  action_register
    ~service:join_project_service
    (fun user () project -> (
        lwt _ = QUser.attach project user.id in
        Lwt.return ()
      ))


(** Project list *)
let make_projects_list user = 
  lwt projects = QProject.fetch_all () 
  and my_projects = QProject.fetch_by_user user in
  let make_button id name =
    if List.exists (fun (i,_) -> i = id) my_projects 
    then button 
        ~a:(classes ["btn"; "disabled"]) 
        ~button_type:`Button [pcdata "Already in !"]
    else join_project_button id
  in
  let aux (id, name, desc) =
    ((dt [span ~a:[a_class ["btn-group"]] [
            make_link_project_page id name ; 
            make_button id name
          ]],[]),
     (dd [pcdata desc],[]))
  in
  Lwt.return (List.map aux projects)



(** Planet related widget *)

let make_free_planet_list project_id =
  lwt users_list = QPlanet.fetch_free_by_user project_id in
  let aux (_id,user_name,count) = 
    li [pcdata (Printf.sprintf "%s : %Li free planets" user_name count)]
  in 
  Lwt.return (ul (List.map aux users_list))

let format_planet_list list = 
  let aux_planet (id,info,prod) =
    i
      ~a:[lclasses ["planet"]]
      [] in
  List.map aux_planet list

let format_grouped_planet_list format_group list = 
  let aux_group (group, l) = 
    (dt [format_group group],[]),(dd (format_planet_list l),[])
  in
  divc "planet-list" [dl ~a:(classe "dl-horizontal") (List.map aux_group list)]

let make_planet_list_by_project user = 
  lwt projects = QProject.fetch_by_user user in
  let aux (id,name) = 
    lwt planets = QPlanet.fetch_by_project_user id user in
    Lwt.return ((id,name), planets)
  in 
  Lwt_list.map_s aux projects
