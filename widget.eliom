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
open Utility

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

{client{
module PopActions = struct 
  
  open Popover

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
  let module H = HoverGroup (PopActions) in
  H.get_init
}}

{shared{
type aux = Dom_html.eventTarget Js.t -> 
  Dom_html.element Js.t option -> unit
}}

let format_planet init format_info (position,info,typ) = 
  let planet = 
	i 
      ~a:[a_title position; lclasses [typ;"planet"]]
      [] 
  in
  ignore {unit{ 
	  let planet = Html5.To_dom.of_i %planet in
	  Popover.apply_with_html
		~content:(Html5.To_dom.of_div (Html5.F.div [ %(format_info info) ])) 
		~trigger:Manual
		planet ;
	  %init 
		  (planet :> Dom_html.eventTarget Js.t) 
		  (Some planet :> Dom_html.element Js.t option)
	}} ;
  planet 

let format_planet_list init format_info list =
  List.map (format_planet init format_info) list

let format_grouped_planet_list format_group format_info list =
  let init = {aux{ get_init_planet () }} in
  let aux_group (group, l) =
	let planets =
	  match l with
		| [] -> [span []]
		| _ -> format_planet_list init format_info l
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
		(Helpers.proj_opt p)
  	in Lwt.return (system,(name,(project,product,note),typ))
  in
  let format_group x = [pcdata x] in
  let format_info (proj,prod,note) = 
	let open Html5.D in
	let proj = opt_map_list
		(fun p -> [pcdata "Project : " ; make_link_member_project p ; br ()]) proj in
	let product = opt_map_list (fun p -> [pcdata ("Product : "^p) ; br ()]) prod in
	let note = opt_map_list (fun n -> [pcdata n]) note in 
	div (proj @ product @ note)
  in 
  user_planet_list_grouped ~arrange ~format_group ~format_info user_id

let make_planet_list_by_project user_id =
  let arrange p =
	lwt (name,typ,system) = Sdd.get_info (Helpers.loc p) in
	lwt project = match (Helpers.proj_opt p) with
		None -> Lwt.return None
	  | Some x -> 
		  lwt name = QProject.get_name x in 
		  Lwt.return (Some (x,name))
	in Lwt.return (project,(name,(),typ))
  in
  let format_group = function
	| None -> [pcdata "Unnafiliated"]
	| Some p -> [make_link_member_project p] 
  in 
  let format_info () = Html5.F.div [] in
  user_planet_list_grouped ~arrange ~format_group ~format_info user_id

let make_free_planet_list project_id =
  lwt users_list = QPlanet.fetch_free_by_user project_id in
  let aux (_id,user_name,count) = 
    li [pcdata (Printf.sprintf "%s : %Li free planets" user_name count)]
  in 
  Lwt.return (ul (List.map aux users_list))
