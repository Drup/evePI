{shared{
open Eliom_lib
open Eliom_content
open Eliom_service
open Eliom_content.Html5

open Widget
open F

open Bootstrap
}}

open Auth
open Skeleton
open Skeleton.Connected
open EvePI_db
open Utility


(** Service to create a project *)
let create_project_service =
  Eliom_service.post_coservice
    ~fallback:project_list_service
    ~post_params:Eliom_parameter.(
		string "name" ** 
		  string "description" ** 
		  int32 "goal") ()

let _ = 
  action_with_redir_register 
    ~redir:project_list_service
    ~service:create_project_service
    (fun admin () (project,(desc,goal)) -> (
        lwt project_id = QProject.create project desc in
        lwt _ = QUser.attach project_id admin.id in
        lwt _ = QAdmin.promote project_id admin.id in
        lwt tree = 
          Tree.make (fun id -> (Sdd.get_sons id) >|= List.map fst) goal in 
        lwt _ = QProject.fill_tree project_id tree in
        Lwt.return ()
      ))

let new_project_form () = 
  lwt goals = Sdd.get_possible_goals () in
  let (ghd,gtl) = 
    let f (id,name) = 
      Option ([],id,Some (pcdata name),true) in
    f (List.hd goals), List.map f (List.tl goals)
  in
  let fun_form (name,(desc,goal)) = 
	[divcs ["input-prepend";"input-append"] 
	   [ string_input ~a:[a_placeholder "Name"] 
		   ~input_type:`Text ~name:name () ;
		 string_input ~a:[a_placeholder "Description"] 
		   ~input_type:`Text ~name:desc () ;
		 int32_select ~name:goal ghd gtl ;
		 button 
		   ~a:(classes ["btn"]) 
		   ~button_type:`Submit [pcdata "Create"] ;
	   ]] in 
  Lwt.return (
	post_form ~a:(classe "form-inline")
	  ~service:create_project_service
	  fun_form ())

(* Nouvelle planete *)

let list_to_select s list = 
  let list = List.map (fun (id,name) -> Option ([],Some id,Some (pcdata name),true)) list in
  let head = Option ([],None,Some (pcdata s),true) in
  head,list

let list_to_raw_select list = 
  let list = List.map (fun (id,name) -> Raw.option (Raw.pcdata name)) list in
  list


let opt_int64 = 
  let to_string = function None -> "" | Some x -> Int64.to_string x in
  let of_string = function "" -> None | s -> Some (Int64.of_string s) in
  Eliom_parameter.user_type ~to_string ~of_string

let rpc_get_planets_by_system =
  server_function Json.t<string> Sdd.get_planets_by_system

let new_planet_service =
  Eliom_service.post_coservice'
    ~post_params:Eliom_parameter.(opt_int64 "project" ** int32 "location") ()

{client{

let select_system_handler slist location planet_div select_system =
  let open Lwt_js_events in
  let select_system = Html5.To_dom.of_input select_system in
  let slist = Js.array (Array.of_list (List.map Js.string slist)) in
  let updater s =
    let current_system = Js.to_string s in
    lwt list = 
      %rpc_get_planets_by_system current_system in
    let list = List.map 
        (fun (id,name,typ) -> 
		   Option ([],id,Some (pcdata (name ^ " ("^typ^")")),true)) list in
    let head,tail = match list with
      | [] -> Option ([],0l,Some (pcdata "No planets !"),false),[]
      | hd::tl -> hd,tl in
    let planet_select = int32_select ~name:location head tail in
    let _ = Html5.Manip.replaceAllChild planet_div [planet_select] in
    Lwt.return () in 
  let updater s = Lwt.ignore_result (updater s) ; s in
  Lwt.async (fun () -> 
    Typeahead.apply 
      ~source:slist  
      ~items:6
      ~updater
      select_system ; 
    Lwt.return () )
}}

let new_planet_form user =
  lwt phead,plist = QProject.fetch_by_user user >|= list_to_select "No project" in
  lwt slist = Sdd.get_systems () >|= List.map snd in
  let form_fun (proj,location) =
    let select_system = 
      D.Raw.input ~a:[a_input_type `Text; 
                      a_autocomplete `Off;
                      a_placeholder "Location"] () in
    let planet_place = D.span [] in
    let _ = {unit{ 
        select_system_handler 
        %slist %location 
        %planet_place %select_system }} in
    [ 
	  divcs ["input-prepend";"input-append"] 
		[ user_type_select 
			(function None -> "" | Some x -> Int64.to_string x)
			~name:proj phead plist ;
		  select_system ;
		  planet_place ;
		  button ~a:(classes ["btn"]) ~button_type:`Submit [pcdata "Create"] ;
		]]
  in
  Lwt.return (
	post_form ~a:(classe "form-inline")
	  ~service:new_planet_service
	  form_fun ())

let _ =
  action_register
    ~service:new_planet_service
    (fun user () (project,location) -> (
        lwt _ = QPlanet.create ?project user.id location in
        Lwt.return ()
      ))

(* Attacher une planete à un project *)

let attach_planet_service =
  Eliom_service.post_coservice'
    ~post_params:Eliom_parameter.(int64 "planet" ** int64 "project") ()

let attach_planet_form user planet =
  lwt project_list = QProject.fetch_by_user user in
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
        lwt _ = QPlanet.update_project planet project in
        Lwt.return ()
      ))

(* Produire sur une planete *)

let specialize_planet_service = 
  Eliom_service.post_coservice'
    ~post_params:Eliom_parameter.(radio int64 "planet" ** int64 "product") ()

let make_product_button form_prod (product_id,product_name,note) =
  D.int64_button ~name:form_prod ~value:product_id [pcdata product_name]

let get_project_tree form_prod project = 
  lwt roots_id = QProject.get_roots project in
  let nodes = Hashtbl.create 20 in
  lwt trees = 
    Tree.make_forest (fun (id,_,_) -> QProject.get_sons id) roots_id in
  let format_node (id, typeid, note) =
    lwt name = Sdd.get_name typeid in
    let button = make_product_button form_prod (id,name,note) in
    Hashtbl.add nodes id button ;
    Lwt.return [button]
  in
  lwt list = Tree.Lwt.printf format_node trees in
  Lwt.return (nodes, list)

{client{

module LightActions = struct
  let class_node_hover = "tree-hover"

  let class_node_select = "tree-select"

  type t = Dom_html.element Js.t

  let up node_class node = 
    node##classList##add(Js.string node_class)

  let down node_class node = 
	node##classList##remove(Js.string node_class)

  let on_hover _ node = 
	up class_node_hover node ; Lwt.return () 

  let on_leave _ node = 
	down class_node_hover node ; Lwt.return ()

  let on_dummy current = match !current with 
      Some cur_b -> down class_node_select cur_b ; Lwt.return ()
    | None -> Lwt.return ()

  let on_click current node = 
    lwt _ = on_dummy current in 
    current := Some node ;
    up class_node_select node ;
	Lwt.return ()

end

let get_init_planet () = 
  hovergroup_get_init (module LightActions)
}}		 

{shared{
type aux = Dom_html.eventTarget Js.t -> 
  Dom_html.element Js.t option -> unit
}}

let make_planet_list_form form_planet nodes project =
  lwt planets_lists = QPlanet.fetch_by_project project in
  let init_planet = {aux{ get_init_planet () }} in
  let make_planet_button (planet_id,node_id) = 
    let node = hashtbl_find nodes node_id in
    let planet = D.int64_radio ~name:form_planet ~value:planet_id () in
    ignore {unit{ 
		%init_planet 
			(To_dom.of_input %planet :> Dom_html.eventTarget Js.t) 
			(opt_map To_dom.of_button %node :> Dom_html.element Js.t option) 
	  }} ;
    planet in
  let aux_users (user, planet_list) =
    let free_planets, planets = 
      List.partition (fun (_,x) -> x = None) planet_list in
    divcs ["planet-list";"control-group"] [
      label 
        ~a:(a_for form_planet :: classe "control-label") 
        [pcdata user];
      divc "controls" [
        span (List.map make_planet_button planets) ;
        span (List.map make_planet_button free_planets) ;
      ]
    ]
  in
  Lwt.return (List.map aux_users planets_lists)

let specialize_planet_form project = 
  lwt project_name = QProject.get_name project in
  let form_fun (form_planet,form_product) = 
    lwt nodes,trees = get_project_tree form_product project in
    lwt planet_list = make_planet_list_form form_planet nodes project in
    Lwt.return (planet_list @ [trees])
  in
  lwt_post_form 
    ~a:(classe "form-horizontal")
    ~service:specialize_planet_service 
    form_fun ()

let _ =
  action_register
    ~service:specialize_planet_service
    (fun user () (planet,product) -> (
        match planet with
            None -> Lwt.return ()
          | Some planet -> 
              lwt _ = QPlanet.update_product planet product in
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
  lwt projects = QProject.fetch_by_admin user in
  let aux (id,name) = 
    li [make_admin_project_link id name]
  in 
  Lwt.return (ul (List.map aux projects))

(** Tools to make the usual layout of an evePI page *)

let stitlebar ?(h=h3) title content = 
  STitle.simple (
	(li [h title]) ::
	  content 
  )

let stitle ?(h=h3) title =
  div ~a:[a_class ["section-title"]] [h title]

(** Le menu *)
let menu user =
  let elements =
    [main_service, [pcdata "Home"] ;
     project_list_service, [pcdata "Projects List"] ;
    ]
  in
  lwt projects = QProject.fetch_by_user user in
  let projects = 
	List.map (fun x -> li [make_link_member_project x]) projects in
  let projects = match projects with
	| [] -> []
	| _ -> [ Dropdown.nav [pcdata "My Projects"; caret] projects ]
  in 
  Lwt.return (navbar 
      ~classes:["navbar-static-top"]
      ~head:[pcdata "Eveπ"]
      [menu 
         ~classes:["nav"] 
         elements 
         ~postfix:projects
         () ;
       disconnect_button ;
      ])


let make_page ?(css=[]) user title body =
  lwt menu = menu user in
  Lwt.return (
	make_page 
	  ~css:(["css";"evePI.css"]::css)
	  title
	  (menu :: [ divcs ["main";"container"] body ]))

(** The real thing *)

let () =
  Connected.register
    ~service:main_sort_service
    (silly (fun sort () user ->
		lwt form = new_planet_form user.id in
		lwt planets = make_planet_list sort user.id in
		make_page 
		  user.id
		  evepi
		  [ center 
			  [h1 ~a:(classe "text-center") [pcdata ("Welcome to "^evepi)] ];
			stitlebar [pcdata "My planets"] [
			  STitle.divider () ;
			  li [pcdata "Sort by : "] ;
			  dropdown_sort sort ;
			] ;
			planets ;
			stitle [pcdata "Register a new planet"] ; 
			form ;
		  ]
	  )) ;

  Connected.register 
    ~service:project_list_service
    (silly (fun () () user ->
        lwt projects_list = make_projects_list user.id in
        lwt project_form = new_project_form () in
        make_page
          user.id
          "Eveπ - Projects"
          [ stitlebar ~h:h3
			  [pcdata "They want "; 
			   em [pcdata "you"] ; 
			   pcdata " in those projects !"]
			  [ STitle.divider () ; 
				li [Collapse.a "create_project_form" [pcdata "Create a new project"]]  ]
			;
			Collapse.div "create_project_form" [project_form] ;
            dl ~a:(classes []) projects_list ;
          ]
      )) ;

  Connected.register
    ~service:project_member_coservice
    (silly (fun project () user ->
        let not_exist () = 
          make_page
            user.id
            "Eveπ"
            [ center [h2 [pcdata "This project doesn't exist"]]
            ] in
        let not_attached () = 
          lwt project_name = QProject.get_name project in
          make_page
            user.id
            ("Eveπ - Project : "^ project_name)
            [ center [
				 h2 [pcdata "Project : " ; 
					 em [pcdata project_name]] ;
				 h3 [pcdata "You are not attached to this project " ; 
					 join_project_button project]
			   ]
            ] in
        let regular_page () =
          lwt project_name = QProject.get_name project in
          lwt is_admin = QAdmin.verify project user.id in
          lwt trees = Qtree.decorate project in
          let admin_link = 
            if is_admin then
              [ STitle.divider () ;
				li [a ~service:project_admin_service 
					  [pcdata "go to the admin panel"] project ]]
            else [] 
          in
          make_page
            user.id
            ("Eveπ - Project : "^ project_name)
            [ stitlebar ~h:h3
				[pcdata "Project : "; 
				 make_link_member_project (project,project_name) ]
				admin_link ;
              trees ;
            ] in
        lwt exist = QProject.exist project in
        if not exist then 
          not_exist () 
        else
          lwt is_attached = QUser.is_attached project user.id in 
          if not is_attached then
            not_attached ()
          else
            regular_page ()
      )) ;

  Connected.register
    ~service:project_admin_service
    (silly (fun project () user ->
        let not_exist () = 
          make_page
            user.id
            "Eveπ"
            [ center [h2 [pcdata "This project doesn't exist"]]
            ] in
        let not_admin () = 
          lwt project_name = QProject.get_name project in
          make_page 
            user.id
            ("Eveπ - Oups") 
            [ center [h2 [pcdata "Hey, you should'nt be here"]] ;
              center [h1 [pcdata "GO AWAY !"]] ] in
        let regular_page () = 
          lwt project_name = QProject.get_name project in
          lwt roots_id = QProject.get_roots project in
          lwt planets = specialize_planet_form project in
		  lwt add_goal = add_goal_form project in 
		  let link_project = make_link_member_project (project,project_name) in 
		  let editable_name = editable_name 
			  ~default_name:project_name link_project change_name_service project in 
          make_page
            user.id
            ("Eveπ - Admin panel - Project : "^ project_name)
            [ stitlebar ~h:h3
				[pcdata "Admin panel - Project : "; 
				 editable_name ]
				[ STitle.divider () ; 
				  li [Collapse.a "add_goal_form" [pcdata "Add a new goal"]]  ]
			  ;
			  Collapse.div "add_goal_form" [add_goal] ;
              planets ;
            ] in
        lwt exist = QProject.exist project in
        if not exist then 
          not_exist () 
        else
          lwt is_admin = QAdmin.verify project user.id in
          if not is_admin then
            not_admin ()
          else
            regular_page ()
      )) 

