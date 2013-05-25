{shared{
open Eliom_lib
open Eliom_content
open Eliom_service
open Eliom_content.Html5
open Bootstrap
}}

open Skeleton
open Skeleton.Connected
open Auth
open EvePI_db
open Utility

{shared{
include Wgeneral
open F
}}


(** {1 Project} *)

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


(** {2 Project adminstration widgets *)

(** Add a new goal to a project *)

let add_goal_service = 
  Eliom_service.post_coservice'
	~post_params:Eliom_parameter.(int64 "project" ** int32 "goal") ()


let add_goal_form project_id = 
  lwt goals = Sdd.get_possible_goals () in
  let (ghd,gtl) = 
    let f (id,name) = 
      Option ([],id,Some (pcdata name),true) in
    f (List.hd goals), List.map f (List.tl goals)
  in
  let form_fun (project_form,goal_form) = 
	[ divc "input-append"
		[ int32_select ~name:goal_form ghd gtl ;
		  int64_button 
			~a:(classes ["btn"])
			~name:project_form
			~value:project_id
			[pcdata "Add"] 
		]]
  in 
  Lwt.return (
	post_form 
	  ~a:(classe "form-inline")
	  ~service:add_goal_service
	  form_fun ()
  )

let _ = 
  action_with_redir_register 
    ~service:add_goal_service
    (fun admin () (project_id,goal) -> 
	   lwt is_admin = QAdmin.verify project_id admin.id in
	   if is_admin then
         lwt tree = 
		   Tree.make (fun id -> (Sdd.get_sons id) >|= List.map fst) goal in 
         lwt _ = QProject.fill_tree project_id tree in
         Lwt.return ()
	   else 
		 Lwt.return ()
    )

(** Change the name of a project *)

let change_name_service = 
  Eliom_service.post_coservice'
	~post_params:Eliom_parameter.(int64 "project" ** string "name") ()

let change_name_form project_id = 
  let form_fun (project_form,name_form) = 
	[ divc "input-append"
		[ string_input ~a:[a_placeholder "New name"] 
			~input_type:`Text ~name:name_form () ;
		  int64_button 
			~a:(classes ["btn"])
			~name:project_form
			~value:project_id
			[pcdata "Change name"] 
		]]
  in 
  Lwt.return (
	post_form 
	  ~a:(classe "form-inline")
	  ~service:change_name_service
	  form_fun ()
  )

let _ = 
  action_with_redir_register 
    ~service:change_name_service
    (fun admin () (project_id,name) -> 
	   lwt is_admin = QAdmin.verify project_id admin.id in
	   if is_admin then
         lwt _ = QProject.update_name project_id name in
         Lwt.return ()
	   else 
		 Lwt.return ()
    )

(** {1 Planet widgets} *)

(** Delete a planet *)

let delete_planet_service = 
  Eliom_service.post_coservice'
	~post_params:Eliom_parameter.(int64 "planet") ()

let delete_planet_link planet = 
  Raw.a ~a:[ 
	a_title "Delete this planet" ; 
	a_class ["link"] ;
	a_onclick {{ 
		fun _ -> Eliom_client.exit_to 
			~service:%delete_planet_service () (%planet)
	  }}]
	[icon ~white:true "remove"]

let _ = 
  action_register
	~service:delete_planet_service
	(fun user () planet -> 
	   lwt is_user = QPlanet.is_attached planet user.id in 
	   if is_user then
		 lwt _ = QPlanet.delete planet in 
		 Lwt.return ()
	   else 
		 Lwt.return ())


(** Change the project of a planet *)

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
  action_register
	~service:change_project_service
	(fun user () (planet,project) -> 
	   lwt is_user = QPlanet.is_attached planet user.id in 
	   if is_user then
		 lwt _ = QPlanet.update_project planet project in 
		 Lwt.return ()
	   else 
		 Lwt.return ())


{client{
module PopActions = struct 
  
  open Tooltip

  type t = Dom_html.element Js.t

  let on_hover current target = match !current with 
	| Some _ -> Lwt.return ()
	| None -> Lwt.return (show target) 

  let on_leave current target = match !current with
	| Some _ -> Lwt.return ()
	| None -> Lwt.return (hide target)

  let on_click current target = 
	(match !current with
	  | Some cur when cur == target -> hide target ; current := None
	  | Some cur -> hide cur ; show target ; current := Some target 
	  | None -> current := Some target) ;
	Lwt.return ()

  let on_dummy current =
	current := None ;
	Lwt.return ()
   
end

let get_init_planet = 
  hovergroup_get_init (module PopActions) 
}}

{shared{
type aux = Dom_html.eventTarget Js.t -> 
  Dom_html.element Js.t option -> unit
}}

let format_planet format_info (position,id,info,typ) = 
  let planet = 
	i 
      ~a:[a_class [typ;"planet"]]
      [] 
  in
  let tooltip = 
	Popover_html.(layout "planettip" (Some Right)
		[pcdata position ; delete_planet_link id] (format_info info)) in
  [planet ; tooltip]

let format_planet_list format_info list =
  ignore {unit{ 
	  Tooltip.apply 
		~position:(`Center,`Right)
		".planet" }} ;
  List.concat (List.map (format_planet format_info) list)

let format_grouped_planet_list format_group format_info list =
  let aux_group (group, l) =
	let planets =
	  match l with
		| [] -> [span []]
		| _ -> format_planet_list format_info l
	in
    (dt (format_group group),[]),(dd planets,[])
  in
  divc "planet-list" [dl ~a:(classe "dl-horizontal") (List.map aux_group list)]

let user_planet_list_grouped ~arrange ~format_group ~format_info user_id = 
  lwt planet_list = QPlanet.fetch_by_user user_id in
  lwt planet_list = Lwt_list.map_s arrange planet_list in
  let planet_list = list_grouping_sort planet_list in
  Lwt.return (
  	format_grouped_planet_list
  	  format_group
  	  format_info
  	  planet_list)

let make_planet_list_by_loc user_id = 
  let arrange p =
    lwt (name,typ,system) = Sdd.get_info (Helpers.loc p) in
	let note = (Helpers.note_opt p) in
	lwt product = 
	  lwt_opt_map 
		(fun p -> lwt tip = get_typeid p in Sdd.get_name tip) 
		(Helpers.prod_opt p) in 
	lwt project = 
	  lwt_opt_map 
		(fun x -> lwt name = QProject.get_name x in Lwt.return (x,name))
		(Helpers.proj_opt p) in
	lwt all_projects = QProject.fetch_by_user user_id in 
	Lwt.return 
	  (system,
	   (name,Helpers.id p,
		(Helpers.id p,project,all_projects,product,note),
		typ))
  in
  let format_group x = [pcdata x] in
  let format_info (planet_id,proj,all_proj,prod,note) = 
	let open Html5.D in
	let all_proj = 
	  li ~a:[a_class ["disabled"]] [Raw.a [pcdata "Change project"]] ::
		List.map (fun x -> li [change_project_link planet_id x]) all_proj in
	let proj = 
	  [pcdata "Project : " ; 
	   divc "dropdown" (
		 (match proj with 
			 Some p -> make_link_member_project p
		   | None -> pcdata "None") 
		 :: (Dropdown.a [icon ~white:true "edit"] all_proj))
	  ] in
	let product = opt_map_list (fun p -> 
		[br () ; pcdata ("Product : "^p)]) prod in
	let note = opt_map_list (fun n -> [br () ; pcdata n]) note in 
	(proj @ product @ note)
  in 
  user_planet_list_grouped ~arrange ~format_group ~format_info user_id

let make_planet_list_by_project user_id =
  let arrange p =
    lwt (name,typ,system) = Sdd.get_info (Helpers.loc p) in
	let note = (Helpers.note_opt p) in
	lwt product = 
	  lwt_opt_map 
		(fun p -> lwt tip = get_typeid p in Sdd.get_name tip) 
		(Helpers.prod_opt p) in 
	lwt project = 
	  lwt_opt_map 
		(fun x -> lwt name = QProject.get_name x in Lwt.return (x,name))
		(Helpers.proj_opt p) in
	lwt all_projects = QProject.fetch_by_user user_id in 
	Lwt.return (
	  project,
	  (name,Helpers.id p,
	   (Helpers.id p,project,all_projects,product,note),
	   typ))
  in
  let format_group = function
	| None -> [pcdata "Unafiliated"]
	| Some p -> [make_link_member_project p] 
  in 
  let format_info (planet_id,proj,all_proj,prod,note) = 
	let open Html5.D in
	let all_proj = 
	  li ~a:[a_class ["disabled"]] [Raw.a [pcdata "Change project"]] ::
		List.map (fun x -> li [change_project_link planet_id x]) all_proj in
	let proj = 
	  [pcdata "Project : " ; 
	   divc "dropdown" (
		 (match proj with 
			 Some p -> make_link_member_project p
		   | None -> pcdata "None") 
		 :: (Dropdown.a [icon ~white:true "edit"] all_proj))
	  ] in
	let product = opt_map_list (fun p -> 
		[br () ; pcdata ("Product : "^p)]) prod in
	let note = opt_map_list (fun n -> [br () ; pcdata n]) note in 
	(proj @ product @ note)
  in 
  user_planet_list_grouped ~arrange ~format_group ~format_info user_id

let make_planet_list choice user_id = match choice with
  | Some "project" -> 
	  make_planet_list_by_project user_id
  | Some "loc" ->
	  make_planet_list_by_loc user_id
  | _ ->
	  make_planet_list_by_loc user_id

let possible_sort = [ "project" ; "location" ]

let dropdown_sort current = 
  let current = match current with 
	  Some s when List.mem s possible_sort -> s
	| _ -> "location"
  in 
  Dropdown.nav [pcdata current; caret]
	(List.map (fun s -> li [a ~service:main_sort_service [pcdata s] (Some s)])
	   possible_sort)

let make_free_planet_list project_id =
  lwt users_list = QPlanet.fetch_free_by_user project_id in
  let aux (_id,user_name,count) = 
    li [pcdata (Printf.sprintf "%s : %Li free planets" user_name count)]
  in 
  Lwt.return (ul (List.map aux users_list))
