open Eliom_lib
open Eliom_content

open Eve_db.PI
					
(* Tables pour la gestion des utilisateurs *)

let users_id_seq = <:sequence< bigserial "users_id_seq" >>
let users = 
  <:table< users (
	id bigint NOT NULL DEFAULT(nextval users_id_seq),
	name text NOT NULL,	  
	password text NOT NULL
	) >>

let projects_id_seq = 
  <:sequence< bigserial "projects_id_seq" >>
let projects = 
  <:table< projects (
	id bigint NOT NULL DEFAULT(nextval projects_id_seq),
	name text NOT NULL,
	description text NOT NULL
	) >>

let projects_tree_product_id_seq = 
  <:sequence< bigserial "projects_tree_product_id_seq" >>
let projects_tree =
  <:table< projects_tree (
	project_id bigint NOT NULL,
	product_id bigint NOT NULL DEFAULT(nextval projects_tree_product_id_seq),
	parent_id bigint,
	notes text
	) >>
	 
let products =
  <:table< products (
	id bigint NOT NULL,
	typeid integer NOT NULL
	) >>
	 
let planets_id_seq = <:sequence< bigserial "planets_id_seq" >>
let planets = 
  <:table< planets (
	id bigint NOT NULL DEFAULT(nextval planets_id_seq),
	user_id bigint NOT NULL,
	project_id bigint,
	product_id bigint,
	location integer NOT NULL,
	notes text
	) >>
	 
let projects_users = 
  <:table< projects_users (
	project_id bigint NOT NULL,
	user_id bigint NOT NULL
	) >>

let projects_admins = 
  <:table< projects_admins (
	project_id bigint NOT NULL,
	user_id bigint NOT NULL
	) >>

(* Quelques fonctions utilitaires *)

let q_currval seq = << { id = currval $seq$ } >>
let get_currval seq = 
  lwt currval = view_one ~log:stderr (q_currval seq) in
  Lwt.return currval#!id

let opt_map f = function
	 None -> None 
  | Some s -> Some (f s)

let opt_string = function 
	 Some s -> s
  | None -> ""

(** Projects *)

let exist_project project = 
  (query
	  <:select< row | 
		row in $projects$ ;
		row.id = $int64:project$ ;
		>>)
  >|= (function [] -> false | _ -> true)

(* Fetch des projets *)
let get_all_projects () =
  (query <:select< row | row in $projects$ >>)
  >|= List.map (fun r -> r#!id, r#!name, r#!description)
					

(* TODO utiliser un outer join ! *)
let get_my_projects id =
  (view
		  << {id = project.id ; name = project.name } |
			activity in $projects_users$ ;
			project in $projects$ ;
			project.id = activity.project_id ;
			activity.user_id = $int64:id$ >>)
  >|= List.map (fun p -> p#!id, p#!name)
					
let get_my_admin_projects id =
  (view
		  << {id = project.id ; name = project.name } |
			activity in $projects_admins$ ;
			project in $projects$ ;
			project.id = activity.project_id ;
			activity.user_id = $int64:id$ >>)
  >|= List.map (fun p -> p#!id, p#!name)


(* Projets creation *)
let new_project name description = 
  lwt _ = query ~log:stderr
					 <:insert< $projects$ :=
					  { id = projects?id ;
					  name = $string:name$ ; 
					  description = $string:description$ ;
					  } >>
  in
  get_currval projects_id_seq
	 
(* Project information *)
let get_project_details project_id =
  (view_one
				<< {name = project.name ; descr = project.description } |
				 project in $projects$ ;
				 project.id = $int64:project_id$ 
				 >>)
  >|= (fun r -> r#!name, r#!descr)

let get_project_name project_id = 
  get_project_details project_id >|= fst
				  
(** Project Tree *)

let fill_project_tree project tree = 
  let new_node parent typeid = 
	 let parent = opt_map (fun p -> <:value< $int64:p$ >>) parent in
	 lwt _ = query 
						<:insert< $projects_tree$ := {
						 project_id = $int64:project$ ;
						 product_id = projects_tree?product_id ;
						 parent_id = of_option $parent$ ;
						 notes = null }				 
						 >> in 
	 lwt currval = get_currval projects_tree_product_id_seq in
	 lwt _ = query 
						<:insert< $products$ := {
						 id = $int64:currval$ ;
						 typeid = $int32:typeid$
						 } >> in
	 Lwt.return currval
  in
  let rec traverse_tree parent tree = match tree with
	 	Tree.Leaf typeid -> 
		lwt _ = new_node parent typeid in Lwt.return ()
	 | Tree.Node (typeid,tl) -> 
		 lwt parent = new_node parent typeid in
		 Lwt_list.iter_s (traverse_tree (Some parent)) tl
  in
  traverse_tree None tree

(* Fetch the project forest *)
let get_roots project = 
  (view 
	  << { id = node.product_id; typeid = node_info.typeid ; note = node.notes} |
		node in $projects_tree$ ;
		node_info in $products$ ;
		node_info.id = node.product_id ;
		node.project_id = $int64:project$ ;
		is_null node.parent_id 
		>>)
  >|= List.map (fun r -> r#!id, r#!typeid, opt_string r#?note)
		  
let get_sons pID = 
  (view
	  << { id = node.product_id; typeid = node_info.typeid ; note = node.notes } |
		node in $projects_tree$ ;
		node_info in $products$ ;
		node_info.id = node.product_id ;
		node.parent_id = $int64:pID$
		>>)
  >|= List.map (fun r -> r#!id,r#!typeid, opt_string r#?note)

(* Modification d'un projet *)
(*
let change_projet_goal project_id new_goal new_tree = 
  lwt _ = 
	 query 
    <:update< row in $projects$ := 
	  { goal = $int32:new_goal$ } |
	  row.project_id = $int64:project_id$
	  >> in
  lwt _ = 
	 delete
	 <:delete< row in $products$ |
	  row.project_id = $int64:
>> 

*)
	 
(** Planets *)

let new_planet ?prod ?note ?project user location =
  let project = opt_map (fun p -> <:value< $int64:p$ >>) project in
  let prod = opt_map (fun p -> <:value< $int64:p$ >>) prod in
  let note = opt_map (fun n -> <:value< $string:n$ >>) note in
  query
	 <:insert< $planets$ := {
	  id = planets?id ;
	  project_id = of_option $project$ ;
	  user_id = $int64:user$ ;
	  product_id = of_option $prod$ ;
	  notes = of_option $note$ ;
	  location = $int32:location$ ;
	  } >>

let update_planet_project planet_id project =
  query
	 <:update< p in $planets$ := { project_id = $int64:project$ } |
	  p.id = $int64:planet_id$ 
	  >>

let update_planet_product planet_id product =
  query
	 <:update< p in $planets$ := { product_id = $int64:product$ } |
	  p.id = $int64:planet_id$ 
	  >>

let update_planet_notes planet_id notes =
  query
	 <:update< p in $planets$ := { notes = $string:notes$ } |
	  p.id = $int64:planet_id$ 
	  >>

(* Group an 'a * 'b list according to the 'a field. 
THE LIST NEED TO BE ORDERED ! *)
let list_grouping l = 
  let aux l (a,b) = match l with 
		[] -> [(a,[b])] 
	 | (x,y)::t when x = a -> (a,b::y)::t
	 | _ -> (a,[b])::l
  in List.fold_left aux [] l

let get_planets_by_user user =
  (view
	  << { p = planet.id } |
		planet in $planets$ ;
		planet.user_id = $int64:user$
		>>)
  >|= List.map (fun x -> x#!p)

let get_planets_by_user_grouped_loc user =
  (view
	<< { proj = planet.project_id ;
		  p = planet.id ; 
		  loc = planet.location ;
        prod = planet.product_id } 
        order by planet.location asc, planet.id asc |
     planet in $planets$ ; 
     planet.user_id = $int64:user$
	 >>)
>|= (fun l -> 
  list_grouping (List.map 
		(fun x -> x#!loc, (x#!p, x#?proj, x#?prod)) l))

let get_planets_by_product product =
  (view
	  << group { p = count[planet.id] ; u = u_ }
		  by { u_ = user.name } |
		planet in $planets$ ;
		user in $users$ ;
		planet.product_id = $int64:product$ ;
		planet.user_id = user.id ;
		>>)
  >|= List.map (fun x -> x#!u,Int64.to_int x#!p)

let get_planets_by_project project =
  (view
		  << { u = user.name  ; 
			    p = planet.id ; 
			    prod = planet.product_id ; } 
			order by user.name desc, planet.id asc |
			planet in $planets$ ;
			user in $users$ ;
			planet.user_id = user.id ;
			planet.project_id = $int64:project$
			>>)
  >|= (fun l -> 
		 list_grouping (List.map (fun x -> x#!u, (x#!p, x#?prod)) l))

let get_planets_by_project_user project user =
  (view ~log:stderr
		  << { p = planet.id ; 
		  loc = planet.location ;
        prod = planet.product_id } |
			planet in $planets$ ;
			planet.user_id = $int64:user$ ;
			planet.project_id = $int64:project$
			>>)
  >|= List.map (fun x -> x#!p,x#!loc,x#?prod)

let get_free_user project_id = 
  (view ~log:stderr
		  << group { id = u_id ; name = u_name ; planets = count[planet.id] } by {u_id = planet.user_id ; u_name = user.name } |
			planet in $planets$ ;
			user in $users$ ;
			user.id = planet.user_id ;
			planet.project_id = $int64:project_id$ ;
			is_null planet.product_id ;
			>>)
  >|= List.map (fun x -> x#!id, x#!name, x#!planets)

(** Users gestion *)

let exists_user name =
  (view
		  <:view< {password = user.password} |
         user in $users$;
         user.name = $string:name$ >>)
  >|= (function [] -> false | _ -> true)
	 
let get_user_id name = 
  (view_one
				<< {id = user.id } |
				 user in $users$ ;
				 user.name = $string:name$ 
				 >>)
  >|= (fun p -> p#!id)

exception User_allready_exists

let add_user name pwd =
  lwt b = exists_user name in
  if not b then 
	 query
			 <:insert< $users$ :=
			  { id = users?id ;
			  name = $string:name$ ; 
			  password = $string:pwd$  } >>
  else raise_lwt User_allready_exists

let attach_admin project user =
  query
		  <:insert< $projects_admins$ :=
			{ project_id = $int64:project$;
			user_id = $int64:user$ ;
			} >>
		  
(* TODO : make password secure *)
let check_pwd name pwd =
  (view
		  <:view< {password = user.password} |
			user in $users$;
			user.name = $string:name$;
			user.password = $string:pwd$ >>)
  >|= (function [] -> false | _ -> true)

let attach_user project user =
  query 
		  <:insert< $projects_users$ := {
			project_id = $int64:project$ ;
			user_id = $int64:user$ 
			} >>

let is_attached project user =
  (query
			<:select< row |
			 row in $projects_users$ ;
			 row.user_id = $int64:user$ ;
			 row.project_id = $int64:project$ ;
			 >>)
  >|= (function [] -> false | _ -> true)

let is_admin project user = 
  (query
			<:select< row |
			 row in $projects_admins$ ;
			 row.user_id = $int64:user$ ;
			 row.project_id = $int64:project$ ;
			 >>)
  >|= (function [] -> false | _ -> true)

