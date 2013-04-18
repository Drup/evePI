{shared{
open Eliom_lib
open Eliom_content
open Eliom_service
open Eliom_content.Html5.D
}}
open Html
		 
module EvePI_app =
  Eliom_registration.App (
		struct
		  let application_name = "evePI"
		end)

let main_service =
  Eliom_service.service
    ~path:[""] ~get_params:Eliom_parameter.unit ()

(** Gestion de la connexion des utilisateurs **)

type user = {id : int64 ; name : string} 
let user = Eliom_reference.eref ~scope:Eliom_common.default_session_scope None

(* La connexion *)

let connection_service =
  Eliom_service.post_coservice'
    ~post_params:Eliom_parameter.(string "name" ** string "password") ()

(* TODO : faire une redirection en cas d'echec *)
let _ = 
  Eliom_registration.Redirection.register
    ~service:connection_service
    (fun () (name, password) ->
		lwt b = Users.check_pwd name password in
		if b then (
		  lwt id = Users.get_user_id name in
		  lwt _ = Eliom_reference.set user (Some { id ; name }) in
		  Lwt.return Eliom_service.void_coservice'
		) else (
		  Lwt.return Eliom_service.void_coservice'
		)
	 )

(* Création d'un compte *)

let create_account_service =
  Eliom_service.post_coservice
    ~fallback:main_service
    ~post_params:Eliom_parameter.(string "name" ** string "password") ()
	 
let _ =
  Eliom_registration.Action.register
    ~service:create_account_service
    (fun () (name, pwd) -> Users.add_user name pwd)
	 
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

(* La deconnexion *)

let disconnection_service =
  Eliom_service.post_coservice'
	 ~post_params:Eliom_parameter.unit ()

let disconnect_button =
  post_form ~a:(classes ["navbar-form";"pull-right"]) ~service:disconnection_service
	 (fun () -> [button ~a:(classes ["btn"]) 
						~button_type:`Submit [pcdata "Log out"] ]) ()

let _ = 
  Eliom_registration.Redirection.register
    ~service:disconnection_service
    (fun () () -> 
	  ignore (Eliom_state.discard ~scope:Eliom_common.default_session_scope ()) ;
	  Lwt.return (Eliom_service.void_coservice')
	 )

(** Le Module Connected
 Permet de considérer qu'on est toujours connecté et servir une page par defaut quand ce n'est pas le cas *)

(* La page par défaut *)
let default_content () =
  make_page "EvePI" [
    container
		 ~head:[h1 ~a:(classe "text-center")  [pcdata "Welcome to EvePI"]]
	    [ 6, [ 
			  h2 ~a:(classe "text-center") [pcdata "Log in"] ;
			  login_name_form connection_service "Connect" ;
			] ;
			6, [
			  h2 ~a:(classe "text-center") [pcdata "Create account"] ;
			  login_name_form create_account_service "Create account" ;
			];
		 ]]

module Connected_translate =
struct
  type page = user -> EvePI_app.page Lwt.t
  let translate page =
    match_lwt Eliom_reference.get user with
      | None -> default_content ()
      | Some user -> page user
end

module Connected =
  Eliom_registration.Customize ( EvePI_app ) ( Connected_translate )

let action_register action =
  let f a b = 
	 match_lwt Eliom_reference.get user with
	 | None ->  Lwt.return ()
	 | Some user -> action user a b
  in
  Eliom_registration.Action.register f

let action_with_redir_register ?(redir=Eliom_service.void_coservice') action =
  let f a b = 
	 match_lwt Eliom_reference.get user with
	 | None -> default_content () >>= EvePI_app.send
	 | Some user -> Eliom_registration.Redirection.send ((action user a b) ; redir)
  in
  Eliom_registration.Any.register f




(** Gestion des projets *)

(* La liste de projet - service *)
let project_list_service =
  Eliom_service.service
    ~path:["projects";""]
	 ~get_params:Eliom_parameter.unit ()

(* La création de projet *)
let create_project_service =
  Eliom_service.post_coservice
    ~fallback:project_list_service
    ~post_params:Eliom_parameter.(string "name" ** (string "description" ** int32 "goal")) ()
  
let _ = 
  action_with_redir_register 
	 ~redir:project_list_service
	 ~service:create_project_service
	 (fun admin () (project,(desc,goal)) -> (
		 lwt project_id = Users.new_project project desc in
		 lwt _ = Users.attach_admin project_id admin.id in
		 lwt _ = Users.attach_user project_id admin.id in
		 lwt tree = 
			Tree.make (fun id -> (Sdd.get_sons id) >|= List.map fst) goal in 
		 lwt _ = Users.fill_project_tree project_id tree in
		 Lwt.return ()
	 ))

let new_project_form () = 
  lwt goals = Sdd.get_possible_goals () in
  let (ghd,gtl) = 
	 let f (id,name) = Option ([],id,Some (pcdata name),true) in
	 f (List.hd goals), List.map f (List.tl goals)
  in					
  Lwt.return (post_form ~a:(classe "form-inline")
	 ~service:create_project_service
	 (fun (name,(desc,goal)) -> [ fieldset ~legend:(legend [pcdata "Create a new project"]) [
		 string_input ~a:[a_placeholder "Name"] ~input_type:`Text ~name:name () ;
		 string_input ~a:[a_placeholder "Description"] ~input_type:`Text ~name:desc () ;
		 int32_select ~name:goal ghd gtl ;
		 button ~a:(classes ["btn"]) ~button_type:`Submit [pcdata "Create"] ;
	 ]]) ())

(* Rejoindre un projet *)

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
		 lwt _ = Users.attach_user project user.id in
		 Lwt.return ()
	 ))


(* La liste de projet - html *)
let make_projects_list user = 
  lwt projects = Users.get_all_projects () 
  and my_projects = Users.get_my_projects user in
  let make_button id name =
	 if List.exists (fun (i,_) -> i = id) my_projects 
	 then button ~a:(classes ["btn"; "disabled"]) ~button_type:`Button [pcdata "Already in !"]
	 else join_project_button id
  in
  let aux (id, name, desc) l =
	 ((dt [span ~a:[a_class ["btn-group"]] [
					  button ~button_type:`Button ~a:[a_class ["btn"; "disabled"]] [strong [pcdata name]]; 
					  make_button id name
			]],[]),
	  (dd [pcdata desc],[]))
	 :: l
  in
  Lwt.return (List.fold_right aux projects [])



(** Gestion des planetes *)

let format_planet_list form_planet list = 
  let aux_planet form_planet (id,location,typ) =
	 int64_radio 
		~a:[lclasse typ; a_title location] 
		~name:form_planet 
		~value:id () in
  List.map (aux_planet form_planet) list


(* Nouvelle planete *)

let list_to_select null list = 
  let list = List.map (fun (id,name) -> Option ([],id,Some (pcdata name),true)) list in
  let head = Option ([],null,Some (pcdata ""),false) in
  head,list

let list_to_raw_select list = 
  let list = List.map (fun (id,name) -> Raw.option (Raw.pcdata name)) list in
  list

let planets_in_system_service =
  Eliom_registration.Ocaml.register_post_coservice'
    ~post_params:Eliom_parameter.(string "system")
    (fun () system_id -> 
		Ocsigen_messages.console2 system_id ;
		lwt list = Sdd.get_planets_by_system system_id in
		let list = List.map 
			 (fun (id,name,typ) -> Option ([],id,Some (pcdata name),true)) list in
		let head = Option ([],Int32.zero,Some (pcdata ""),false) in
		Lwt.return (head,list)
	 )

let new_planet_service =
  Eliom_service.post_coservice'
	 ~post_params:Eliom_parameter.(neopt (int64 "project") ** int32 "location") ()

{client{

let select_system_handler location planet_div select_system =
  let open Lwt_js_events in
  let select_system = Html5.To_dom.of_select select_system in
  Lwt.async (fun () -> changes select_system 
		(fun _ _ ->
		  let current_system = Js.to_string select_system##value in 
		  lwt head,list = Eliom_client.call_caml_service 
				~service:%planets_in_system_service () current_system in
		  let planet_select = int32_select ~name:location head list in
		  let _ = Html5.Manip.replaceAllChild planet_div [planet_select] in
		  Lwt.return ()
		))
}}

let new_planet_form user =
  lwt phead,plist = Users.get_my_projects user >|= list_to_select 0L in
  lwt slist = Sdd.get_systems () >|= list_to_raw_select in
  let form_fun (proj,location) =
	 let select_system = Raw.select slist in
	 let planet_place = span [] in
	 ignore {unit{ select_system_handler %location %planet_place %select_system }} ;
	 [ fieldset ~legend:(legend [pcdata "Create a new planet"]) [
		  int64_select ~name:proj phead plist ;
		  select_system ;
		  planet_place ;
		  button ~a:(classes ["btn"]) ~button_type:`Submit [pcdata "Create"] ;
		]]
  in
  Lwt.return (post_form ~a:(classe "form-inline")
		~service:new_planet_service
		form_fun ())
(*
let new_planet_button project_id =
  Raw.a
	 ~a:[a_onclick 
			 {{ fun _ -> 
				 ignore (Eliom_client.change_page  
					  ~service:%new_planet_service () (Some %project_id)) }}]
	 [badge_inverse [pcdata "+"]]
*)
let _ =
  action_register
	 ~service:new_planet_service
	 (fun user () (project,location) -> (
		 lwt _ = Users.new_planet ?project user.id in
		 Lwt.return ()
	 ))

(* Attacher une planete à un project *)

let attach_planet_service =
  Eliom_service.post_coservice'
	 ~post_params:Eliom_parameter.(int64 "planet" ** int64 "project") ()

let attach_planet_form user planet =
  lwt project_list = Users.get_my_projects user in
  let project_list = 
	 List.map (fun (id,name) -> Option ([],id,Some (pcdata name),true)) project_list in
  let head = Option ([],Int64.zero,Some (pcdata ""),false) in
  Lwt.return (post_form ~a:(classe "form-inline")
	 ~service:attach_planet_service
	 (fun (plan,proj) -> [ 
		 int64_select ~name:proj head project_list ;
		 int64_button ~name:plan ~value:planet ~a:(classes ["btn"]) [pcdata "Attach"] ;
	 ]) ())

let _ =
  action_register
	 ~service:attach_planet_service
	 (fun user () (planet,project) -> (
		 lwt _ = Users.update_planet_project planet project in
		 Lwt.return ()
	 ))

(* Produire sur une planete *)
	 
let specialize_planet_service = 
  Eliom_service.post_coservice'
	 ~post_params:Eliom_parameter.(radio int64 "planet" ** int64 "product") ()

let make_planet_list_form project =
  lwt planets_lists = Users.get_planets_by_project project in
  let aux_planets planet planet_id = 
	 int64_radio ~name:planet ~value:planet_id () in
  let aux_users planet (user, planet_list) =
	 divc "control-group" [
			  label ~a:(a_for planet :: classe "control-label") [pcdata user];
			  divc "controls" (List.map (aux_planets planet) planet_list) ;
			]
  in
  Lwt.return (fun planet -> List.map (aux_users planet) planets_lists)

let make_product_button prod (product_id,product_name) =
  [int64_button ~name:prod ~value:product_id [pcdata product_name]]

let specialize_planet_form project = 
  lwt planet_list = make_planet_list_form project in
  lwt project_name = Users.get_project_name project in
  lwt roots_id = Users.get_roots project in
  lwt trees = 
	 Lwt_list.map_s
		(Tree.make (fun (id,_) -> Users.get_sons id)) roots_id in
  lwt trees = 
	 Lwt_list.map_s 
		(Tree.lwt_map 
			(fun (id,typeid) -> 
			  lwt name = Sdd.get_name typeid in Lwt.return (id,name))) trees in
  Lwt.return (post_form ~a:(classe "form-horizontal")
		~service:specialize_planet_service 
		(fun (plan,prod) -> 
		  (planet_list plan) @
		  List.map (Tree.print_as_list (make_product_button prod)) trees
		) ())
	 
let _ =
  action_register
	 ~service:specialize_planet_service
	 (fun user () (planet,product) -> (
		 match planet with
			None -> Lwt.return ()
		 | Some planet -> 
			 lwt _ = Users.update_planet_product planet product in
			 Lwt.return ()
	 ))

(** Administration projet *)
let project_admin_service =
  Eliom_service.service
    ~path:["projects";"admins"]
	 ~get_params:Eliom_parameter.(suffix (int64 "project")) ()

let make_admin_project_link project_id project_name = 
  a ~service:project_admin_service [pcdata project_name] project_id

let make_admin_projects_list user = 
  lwt projects = Users.get_my_admin_projects user in
  let aux (id,name) = 
	 li [make_admin_project_link id name]
  in 
  Lwt.return (ul (List.map aux projects))

let make_free_planet_list project_id =
  lwt users_list = Users.get_free_user project_id in
  let aux (_id,user_name,count) = 
	 li [pcdata (Printf.sprintf "%s : %Li free planets" user_name count)]
  in 
  Lwt.return (ul (List.map aux users_list))
  

(** Mes projets *)
let project_member_service =
  Eliom_service.service
    ~path:["projects";"member";""] 
	 ~get_params:Eliom_parameter.unit ()

let project_member_coservice =
  Eliom_service.service
    ~path:["projects";"member"] 
	 ~get_params:Eliom_parameter.(suffix (int64 "project")) ()

let make_link_member_project project_id project_name = 
  a ~service:project_member_coservice [pcdata project_name] project_id

let make_member_projects_list user = 
  lwt projects = Users.get_my_projects user in
  let aux (id,name) = 
	 li [make_link_member_project id name]
  in 
  Lwt.return (ul (List.map aux projects))

 
(** Le menu *)

let menu () =
  let elements =
	 [main_service, [pcdata "Home"] ;
	  project_list_service, [pcdata "Projets"] ;
	  project_member_service, [pcdata "My Projects"] ;
	 ]
  in
  navbar 
	 ~head:[pcdata "Eve PI"]
	 [menu 
		 ~classes:["nav"] 
		 elements () ; 
	  disconnect_button
	 ]


(** Quelques utilitaires *)

let make_page ?(css=[]) title body =
  make_page 
	 ~css:(["css";"evePI.css"]::css)
	 title
	 [ divc "container" (menu () :: body) ]

(** The real thing *)

let () =
  Connected.register
	 ~service:main_service
	 (fun () () -> 
		Lwt.return (
		  fun user ->
			 lwt form = new_planet_form user.id in
			 make_page 
				"Eve PI"
				[ center [h1 ~a:(classe "text-center") [pcdata "Welcome to EvePI"] ];
				  form ;
				]
		)) ;

  Connected.register 
	 ~service:project_list_service
	 (fun () () ->
		Lwt.return (
		  fun user ->
			 lwt projects_list = make_projects_list user.id in
			 lwt project_form = new_project_form () in
			 make_page
				"Eve PI - Projects"
				[ center [h2 [pcdata "They want "; em [pcdata "you"] ; pcdata " in those projects !"]] ;
				  dl ~a:(classes []) projects_list ;
				  project_form ;
				]
	 )) ;
  
  Connected.register
	 ~service:project_admin_service
	 (fun project () -> 
		Lwt.return (
		  fun user ->
			 lwt project_name = Users.get_project_name project in
			 lwt is_admin = Users.is_admin project user.id in
			 lwt roots_id = Users.get_roots project in
			 lwt planets = specialize_planet_form project in
			 lwt trees = 
				Lwt_list.map_s
				  (Tree.make (fun (id,_) -> Users.get_sons id)) roots_id in
			 lwt trees = 
				Lwt_list.map_s 
				  (Tree.lwt_map (fun (id,typeid) -> Sdd.get_name typeid)) trees in
			 lwt free_planets = 
				make_free_planet_list project in
			 if not is_admin then 
				make_page ("Eve PI - Oups") 
				  [ center [h2 [pcdata "Hey, you should'nt be here"]] ;
					 center [h1 [pcdata "GO AWAY !"]] ]
			 else
				make_page
				  ("Eve PI - Admin - "^ project_name)
				  ( center [h2 [pcdata "Admin panel for the project :" ; em [pcdata project_name] ]] ::
						free_planets ::
						planets ::
						[]
						(*List.map Tree.print_as_plain_list trees*)
				  )
		)) ;
  
  Connected.register
	 ~service:project_member_service
	 (fun () () -> 
	  Lwt.return (
			fun user ->
			lwt project_list = make_member_projects_list user.id in
			make_page
			  "Eve PI - My Projects"
			  [ center [h2 [pcdata "Here are your projects"]] ;
				 project_list
			  ]
	 )) ;

  Connected.register
	 ~service:project_member_coservice
	 (fun project () -> 
		Lwt.return (
		  fun user ->
			 lwt project_name = Users.get_project_name project in
			 lwt roots_id = Users.get_roots project in
			 lwt trees = 
				Lwt_list.map_s
				  (Tree.make (fun (id,_) -> Users.get_sons id)) roots_id in
			 lwt trees = 
				Lwt_list.map_s 
				  (Tree.lwt_map (fun (id,typeid) -> Sdd.get_name typeid)) trees in
			 lwt is_admin = Users.is_admin project user.id in
			 let admin_link = if is_admin then
				  [ a ~service:project_admin_service [badge_important [pcdata "admin panel"]] project ] else [] in
			 make_page
				("Eve PI - My Projects - "^ project_name)
				( center [h2 ([pcdata "Project : " ; em [pcdata project_name] ; pcdata " " ] @ admin_link)] ::
					 List.map Tree.print_as_plain_list trees
				)
		)) ;
  
