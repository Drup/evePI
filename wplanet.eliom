(** This module defines every Planet-related widgets *)

{shared{
open Eliom_lib
open Eliom_content.Html5
}}

open Skeleton
open Auth
open EvePI_db
open Utility

open F

(** {3 Delete a planet} *)

let delete_service = 
  Eliom_service.post_coservice'
    ~post_params:Eliom_parameter.(int64 "planet") ()

let delete_link planet = 
  Raw.a ~a:[ 
    a_title "Delete this planet" ; 
    a_class ["link"] ;
    a_onclick {{ 
	fun _ -> Eliom_client.exit_to 
	    ~service:%delete_service () (%planet)
      }}]
    [Bootstrap.icon ~white:true "remove"]

let _ = 
  Wrap.action_register
    ~service:delete_service
    (fun user () planet -> 
       lwt is_user = QPlanet.is_attached planet user.id in 
       if is_user then
	 lwt _ = QPlanet.delete planet in 
	 Lwt.return ()
       else 
	 Lwt.return ())


(** {3 Change the project of a planet} *)

let change_project_service = 
  Eliom_service.post_coservice'
    ~post_params:Eliom_parameter.(int64 "planet" ** int64 "project") ()

let change_project_link planet (project_id,project_name) = 
  Raw.a ~a:[
    a_class ["link"] ;
    a_onclick {{ 
	fun _ -> Eliom_client.exit_to
	    ~service:%change_project_service () (%planet,%project_id)
      }}]
    [pcdata project_name]

let _ = 
  Wrap.action_register
    ~service:change_project_service
    (fun user () (planet,project) -> 
       lwt is_user = QPlanet.is_attached planet user.id in 
       if is_user then
	 lwt _ = QPlanet.update_project planet project in 
	 Lwt.return ()
       else 
	 Lwt.return ())
