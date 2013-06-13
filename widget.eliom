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
	[pcdata position ; Wplanet.delete_link id] (format_info info)) in
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
    lwt (name,typ,system) = Sdd.get_info (Helpers.loc p) in
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

let make_free_planet_list project_id =
  lwt users_list = QPlanet.fetch_free_by_user project_id in
  let aux (_id,user_name,count) = 
    li [pcdata (Printf.sprintf "%s : %Li free planets" user_name count)]
  in 
  Lwt.return (ul (List.map aux users_list))
