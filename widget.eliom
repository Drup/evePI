{shared{
open Eliom_lib
open Eliom_content.Html5
open Bootstrap
}}

open Skeleton
open Auth
open EvePI_db
open Utility

{shared{
include Wgeneral
open F
}}


let format_grouped_data ?(a=[]) format_group format_data list = 
  let aux_group (group, l) =
    let planets =
      match l with
	| [] -> [span []]
	| _ -> format_data l
    in
    (D.dt (format_group group),[]),(dd planets,[])
  in
  D.dl ~a:((a_class ["dl-horizontal"]) :: a) (List.map aux_group list)
  

(** {1 Planets list on the home page} *)

let format_planet format_info (position,id,info,typ) = 
  let planet = Wplanet.icon typ
  in
  let tooltip = 
    Popover_html.(layout "planettip" (Some Right)
	[pcdata position ; Wplanet.delete_link id] (format_info info)) in
  [planet ; tooltip]

let format_planet_list format_info list =
  ignore {unit{ 
      Tooltip.apply 
	~position:(`Center,`Right)
	".planet" }} ;
  List.concat (List.map (format_planet format_info) list)

let format_grouped_planet_list format_group format_info list =
  let format_list l = format_planet_list format_info l in
  divc "planet-list" [format_grouped_data format_group format_list list]

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
    lwt (name,typ,system) = Sdd.get_planet_info (Helpers.loc p) in
    let note = (Helpers.note_opt p) in
    lwt product = 
      Option.map_lwt 
	(fun p -> lwt tip = get_typeid p in Sdd.get_name tip) 
	(Helpers.prod_opt p) in 
    lwt project = 
      Option.map_lwt 
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
    let open D in
    let all_proj = 
      li ~a:[a_class ["disabled"]] [Raw.a [pcdata "Change project"]] ::
	List.map (fun x -> li [Wplanet.change_project_link planet_id x]) all_proj in
    let proj = 
      [pcdata "Project : " ; 
       divc "dropdown" (
	 (match proj with 
	     Some p -> member_project_link p
	   | None -> pcdata "None") 
	 :: (Dropdown.a [icon ~white:true "edit"] all_proj))
      ] in
    let product = Option.map_list (fun p -> 
	[br () ; pcdata ("Product : "^p)]) prod in
    let note = Option.map_list (fun n -> [br () ; pcdata n]) note in 
    (proj @ product @ note)
  in 
  user_planet_list_grouped ~arrange ~format_group ~format_info user_id

let make_planet_list_by_project user_id =
  let arrange p =
    lwt (name,typ,system) = Sdd.get_planet_info (Helpers.loc p) in
    let note = (Helpers.note_opt p) in
    lwt product = 
      Option.map_lwt 
	(fun p -> lwt tip = get_typeid p in Sdd.get_name tip) 
	(Helpers.prod_opt p) in 
    lwt project = 
      Option.map_lwt 
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
    | Some p -> [member_project_link p] 
  in 
  let format_info (planet_id,proj,all_proj,prod,note) = 
    let open D in
    let all_proj = 
      li ~a:[a_class ["disabled"]] [Raw.a [pcdata "Change project"]] ::
	List.map (fun x -> li [Wplanet.change_project_link planet_id x]) all_proj in
    let proj = 
      [pcdata "Project : " ; 
       divc "dropdown" (
	 (match proj with 
	     Some p -> member_project_link p
	   | None -> pcdata "None") 
	 :: (Dropdown.a [icon ~white:true "edit"] all_proj))
      ] in
    let product = Option.map_list (fun p -> 
	[br () ; pcdata ("Product : "^p)]) prod in
    let note = Option.map_list (fun n -> [br () ; pcdata n]) note in 
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

(** {1 Planets tree} *)


let layout_planets planets = 
  List.map
    (fun (name,typ,notes) -> Wplanet.icon ~name typ)
    planets

let layout_node (name,notes,content) = 
  let content = D.div content in
  let node = 
    D.div
      [D.span ~a:[a_title notes] [pcdata name] ;
       content
      ] in
  node

let layout_admin_node ?(a=[]) (name,notes,planets) = 
  let aux_user ((user_id, user_name), l) =
    let planets =
      match l with
	| [] -> [span []]
	| _ -> List.map snd l
    in
    let container = D.dd planets in
    user_id, container, ((D.dt [pcdata user_name],[]),(container,[]))
  in
  let user_planets = List.map aux_user planets in
  let drag_info = List.map (fun (id,container,_) -> (id,container)) user_planets in
  let content = 
    D.dl ~a:((a_class ["dl-horizontal"]) :: a) 
      (List.map (fun (_,_,x) -> x) user_planets) in
  layout_node (name,notes,[content]) , drag_info

let user_project_tree project user = 
  lwt trees = Qtree.project_trees project in
  let decorate_node (id,typeid,notes) =
    lwt name = Sdd.get_name typeid in 
    lwt planets = QPlanet.fetch_by_node_user id user in
    lwt planets = Lwt_list.map_s
	(fun p -> Sdd.get_planet_info (Sql.get p#loc)) planets in
    let node = layout_node (name,notes,layout_planets planets) in
    Lwt.return [node]
  in
  lwt free_planets = QPlanet.fetch_free ~project ~user in
  lwt free_planets = 
    Lwt_list.map_p 
      (fun p -> Sdd.get_planet_info (Sql.get p#planet_loc)) 
      free_planets in
  let free_planets = layout_planets free_planets in
  lwt tree = Tree.Lwt.printf decorate_node trees in
  Lwt.return [D.div free_planets;tree]

let admin_project_tree project user = 
  lwt trees = Qtree.project_trees project in

  let decorate_node (nodeid,typeid,notes) = 
    lwt name = Sdd.get_name typeid in 
    lwt planets = 
      QPlanet.fetch_by_node nodeid  >>=
      Lwt_list.map_p (fun p -> 
	lwt (name,typ,system) = Sdd.get_planet_info (Sql.get p#loc) in
	Lwt.return (
	  (Sql.get p#user_id, Sql.get p#user_name), 
	  (Sql.get p#id, Wplanet.icon ~name typ)
	))
      >|=  list_grouping
    in
    Lwt.return (nodeid,name,notes,planets)
  in

  let extract_planets planets = 
    List.flatten (
      List.map 
	(fun ((u_id,_),l) -> List.map (fun (p_id,p_icon) -> (p_icon,(p_id,u_id))) l) 
	planets )
  in 

  let drag_node (id,name,notes,planets) = 
    let container, contents = layout_admin_node (name, notes, planets) in
    id, container, contents
  in

  lwt trees = Tree.Lwt.mapf decorate_node trees in
  let drag_trees = Tree.mapf drag_node trees in
  let html_trees = Tree.printf (fun (_,node,_) -> [node]) drag_trees in

  (* We also need to print free planets *)
  lwt free_planets = 
    QPlanet.fetch_free_by_user ~project >>=
    lwt_grouped_map 
      Lwt.return 
      (fun (id,loc) -> 
	 lwt (name,typ,system) = Sdd.get_planet_info loc in
	 Lwt.return (id,Wplanet.icon ~name typ)
      ) in

  (* FIXME the planet extraction is a bit involved (grouping + 2 * flatten ...) *)
  let draggables = 
    extract_planets free_planets @
      List.flatten (Tree.to_listf 
	  (Tree.mapf (fun (_,_,_,p) -> extract_planets p) trees)) in

  let dropzones = Tree.to_listf drag_trees in

  let _ = {unit{
      let draggables = 
	List.map 
	  (fun (p,(pid,uid)) -> debug "%Li %Li" pid uid ; (To_dom.of_element p, (pid,uid)))
	%draggables
      in
      let dropzones =
	List.map 
	  (fun (id, container, user_content) -> 
	     let f planet (_,u_id) = 
	       let user_planets = To_dom.of_dd (List.assoc u_id user_content) in
	       Dom.appendChild user_planets planet 
	     in
	     (To_dom.of_element container, f, id)
	  )
	%dropzones 
      in
      debug "bouh" ;
      draggable_init draggables dropzones 
    }} in

  let free_planets = 
    format_grouped_data 
      (fun (id,name) -> [pcdata name])
      (List.map snd)
      free_planets in

  Lwt.return [ free_planets; html_trees ]

(* let make_free_planet_list project_id = *)
(*   lwt users_list = QPlanet.fetch_free_by_user project_id in *)
(*   let aux (_id,user_name,count) =  *)
(*     li [pcdata (Printf.sprintf "%s : %Li free planets" user_name count)] *)
(*   in  *)
(*   Lwt.return (ul (List.map aux users_list)) *)
