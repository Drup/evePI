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


let icon ?(a=[]) ?name typ = 
  let name = Option.to_list (Option.map a_title name) in
  D.i ~a:((a_class [typ;"planet"]) :: (a_draggable `True) :: name @ a) []

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
       (* Only the user himself can delete his planets *)
       lwt is_user = QPlanet.is_attached planet user.id in 
       if is_user then
	 QPlanet.delete planet
       else 
	 Lwt.return ()
    )


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
       (* Only the user himself can change the project of his planets *)
       lwt is_user = QPlanet.is_attached planet user.id in 
       if is_user then
	 QPlanet.update_project planet project
       else 
	 Lwt.return ()
    )

(** {3 Change the production of a planet} *)

let specialize_service = 
  Eliom_service.post_coservice'
    ~post_params:Eliom_parameter.(int64 "planet" ** int64 "product") ()

let specialize (user,planet,product) = 
  (* Only the admin of the project can specialize a planet *)
  (* FIXME may be desirable to do a single request with a join here *)
  lwt planet_info = QPlanet.get_info planet in
  let project = Sql.getn planet_info#project in
  lwt is_admin = Option.map_lwt (fun p -> QAdmin.verify p user) project in
  if is_admin = Some true then 
    QPlanet.update_product planet product 
  else
    Lwt.return ()

let _ =
  Wrap.unit_register
    ~service:specialize_service
    (fun user () (planet,product) -> 
       specialize (user.id,planet,product)
    )
