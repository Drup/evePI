open Eliom_lib
open Eliom_content
open Eliom_service
open Eliom_content.Html5.D

open Bootstrap
open Auth

let evepi = "Eveπ"

module App =
  Eliom_registration.App (
  struct
    let application_name = "evePI"
  end)

(** Home page *)
let main_sort_service =
  Eliom_service.service
    ~path:[""] ~get_params:Eliom_parameter.(opt (string "sort")) ()

let main_service = 
  Eliom_service.preapply
	~service:main_sort_service None

(** The Default page if you are not logged in *)
let default_content () =
  make_page evepi [
    container
      ~head:[h1 ~a:(classe "text-center")  [pcdata ("Welcome to "^evepi)]]
      [ 6, [ 
          h2 ~a:(classe "text-center") [pcdata "Log in"] ;
          login_name_form connection_service "Connect" ;
        ] ;
        6, [
          h2 ~a:(classe "text-center") [pcdata "Create account"] ;
          login_name_form create_account_service "Create account" ;
        ];
      ]]

module Connected = Connected (struct let v = default_content end) (App)
open Connected

(** Projects list *)
let project_list_service =
  Eliom_service.service
    ~path:["projects";""]
    ~get_params:Eliom_parameter.unit ()

(** Project page *)
let project_member_coservice =
  Eliom_service.service
    ~path:["projects"] 
    ~get_params:Eliom_parameter.(suffix (int64 "project")) ()
