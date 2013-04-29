
open Eliom_lib
open Eliom_content
open Eliom_service
open Eliom_tools
open Eliom_content.Html5
open Eliom_content.Html5.D

open Structure
open EvePI_db
open Bootstrap


(** The connected user *)

type user = {id : int64 ; name : string}
let user = Eliom_reference.eref ~scope:Eliom_common.default_session_scope None

(** The connexion *)

let connection_service =
  Eliom_service.post_coservice'
    ~post_params:Eliom_parameter.(string "name" ** string "password") ()

(* TODO : Do a redirection in case of failure *)
let _ = 
  Eliom_registration.Redirection.register
    ~service:connection_service
    (fun () (name, password) ->
      lwt b = User.check_pwd name password in
      if b then (
        lwt id = User.get_id name in
        lwt _ = Eliom_reference.set user (Some { id ; name }) in
        Lwt.return Eliom_service.void_coservice'
      ) else (
        Lwt.return Eliom_service.void_coservice'
      )
    )

(** {1 Account creation} *)

let create_account_service =
  Eliom_service.post_coservice
    ~fallback:main_service
    ~post_params:Eliom_parameter.(string "name" ** string "password") ()

let create_account_service =
  Eliom_registration.Action.register_post_coservice'
    ~post_params:Eliom_parameter.(string "name" ** string "password")
    (fun () (name, pwd) -> User.create name pwd)

(** Form to create or to connect an account *)
let login_name_form service button_text =
  post_form ~a:(classe "form-horizontal")
    ~service
    (fun (name1, name2) ->
      [divc 
         "control-group" [
         label ~a:(a_for name1 :: classe "control-label") [pcdata "login: "];
         divc "controls" [string_input ~input_type:`Text ~name:name1 ()] ;
       ] ;
       divc 
         "control-group" [
         label ~a:(a_for name2 :: classe "control-label") [pcdata "password: "];
         divc "controls" [string_input ~input_type:`Password ~name:name2 ()] ;
       ] ;
       divc 
         "control-group" [
         divc "controls" 
           [string_input 
              ~a:(classes ["btn";"btn-primary"]) 
              ~input_type:`Submit ~value:button_text ()]
       ] ;
      ]) ()


(** {1 The Disconnexion} *)

let disconnection_service  = 
  Eliom_registration.Redirection.register_post_coservice'
	~post_params:Eliom_parameter.unit
    (fun () () -> 
      lwt _ = 
		Eliom_state.discard 
		  ~scope:Eliom_common.default_session_scope () in 
      Lwt.return (Eliom_service.void_coservice')
    )

let disconnect_button =
  post_form 
    ~a:(classes ["navbar-form";"pull-right"]) 
    ~service:disconnection_service
    (fun () -> [
      button 
        ~a:(classes ["btn";"btn-danger"])
        ~button_type:`Submit [pcdata "Log out"] ]) ()


(** Le Module Connected
    Permet de considérer qu'on est toujours connecté et servir une page par defaut quand ce n'est pas le cas *)

module type Default_content =
sig 
  val v : unit -> Html5_types.html Eliom_content.Html5.elt
end

module Connected_translate ( Default : Default_content) =
struct
  type page = user -> EvePI_app.page Lwt.t
  let translate page =
    match_lwt Eliom_reference.get user with
      | None -> Lwt.return (Default.v ())
      | Some user -> page user
end

module Connected ( Default : Default_content ) =
struct 
  include Eliom_registration.Customize 
	  ( EvePI_app ) 
	  ( Connected_translate (Default) )

  let action_register action =
	let f = 
	  wrap_handler 
		(fun () -> Eliom_reference.get user)
		(fun _ _ -> Lwt.return ())
		action
	in
	Eliom_registration.Action.register f

  let action_with_redir_register ?(redir=Eliom_service.void_coservice') action =
	let f = 
	  wrap_handler 
		(fun () -> Eliom_reference.get user)
		(fun _ _ -> EvePI_app.send (Default.v ()))
		(fun u g p -> 
		  Eliom_registration.Redirection.send ((action u g p) ; redir))
	in
	Eliom_registration.Any.register f

end
