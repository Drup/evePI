(** Various tables and queries related to the PI database *)
(* Indentation with macaque is messy, propositions are welcomed. *)

(**
   Naming convention is the following:
   - fetch for queries that return a list of concernd elements;
   - by_ specify the criteria of selection;
   - group_ specify how results are grouped. Grouped with {! list_grouping};
   - get for queries that return properties on the object.

*)


open Eliom_lib
open Eliom_content

open Utility
open Eve_db.PI



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

let q_currval seq = << { id = currval $seq$ } >>
let get_currval seq = 
  lwt currval = view_one ~log:stderr (q_currval seq) in
  Lwt.return currval#!id



(** {1 Projects} *)
(*****************)

module QProject = struct 
let exist project = 
  (query
	  <:select< row | 
		row in $projects$ ;
		row.id = $int64:project$ ;
		>>)
  >|= (function [] -> false | _ -> true)
  
let update_name project_id name =
  query
	 <:update< p in $projects$ := { name = $string:name$ } |
	  p.id = $int64:project_id$ 
	  >>

let update_descr project_id description =
  query
	 <:update< p in $projects$ := { description = $string:description$ } |
	  p.id = $int64:project_id$ 
	  >>

let fetch_all () =
  (query <:select< project 
	         order by project.id asc | 
		   project in $projects$ >>)
  >|= List.map (fun r -> r#!id, r#!name, r#!description)
	  
let fetch_by_user id =
  (view
   << {id = project.id ; name = project.name } 
	   order by project.id asc |
    activity in $projects_users$ ;
    project in $projects$ ;
    project.id = activity.project_id ;
    activity.user_id = $int64:id$ >>)
  >|= List.map (fun p -> p#!id, p#!name)
					
let fetch_by_admin id =
  (view
   << {id = project.id ; name = project.name } 
	   order by project.id asc |
    activity in $projects_admins$ ;
    project in $projects$ ;
    project.id = activity.project_id ;
    activity.user_id = $int64:id$ >>)
  >|= List.map (fun p -> p#!id, p#!name)

(** Create a new project, return the id of the project *)
let create name description = 
  lwt _ = query ~log:stderr
	  <:insert< $projects$ :=
	 		{ id = projects?id ;
			name = $string:name$ ; 
			description = $string:description$ ;
			} >>
  in
  get_currval projects_id_seq
	 
(** Various project informations *)

let get_details project_id =
  (view_one
				<< {name = project.name ; descr = project.description } |
				 project in $projects$ ;
				 project.id = $int64:project_id$ 
				 >>)
  >|= (fun r -> r#!name, r#!descr)

let get_name project_id = 
  get_details project_id >|= fst
				
  
(** Project tree related queries *)

(** Take an existing tree and add it to the {! projects_tree} table *)
let fill_tree project tree = 
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
		Lwt.return () (* We don't want P0 inside the project_tree *)
	| Tree.Node (typeid,tl) -> 
		lwt parent = new_node parent typeid in
		Lwt_list.iter_s (traverse_tree (Some parent)) tl
  in
  traverse_tree None tree

(** Two functions used to query the project tree recursivly. 
	See also the {! Tree} module.
*)

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

end
	 
let get_typeid product_id =
  (view_one
				<< {typeid = product.typeid } |
				 product in $products$ ;
				 product.id = $int64:product_id$ 
				 >>)
  >|= (fun r -> r#!typeid)


(** {1 Planets} *)
(****************)

module QPlanet = struct 

let create ?prod ?note ?project user location =
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

let update_project planet_id project =
  query
	 <:update< p in $planets$ := { project_id = $int64:project$ } |
	  p.id = $int64:planet_id$ 
	  >>

let update_product planet_id product =
  query
	 <:update< p in $planets$ := { product_id = $int64:product$ } |
	  p.id = $int64:planet_id$ 
	  >>

let update_notes planet_id notes =
  query
	 <:update< p in $planets$ := { notes = $string:notes$ } |
	  p.id = $int64:planet_id$ 
	  >>


let fetch_by_user user =
  view
	  << { id = planet.id ;
		   proj = planet.project_id ;
		   prod = planet.product_id ;
		   loc = planet.location ;
		   note = planet.notes ;
	      } |
		planet in $planets$ ;
		planet.user_id = $int64:user$ ;
		>>

let fetch_by_user_group_loc user =
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

let fetch_by_node product =
  (view
	  << group { p = count[planet.id] ; u = u_ }
		  by { u_ = user.name } |
		planet in $planets$ ;
		user in $users$ ;
		planet.product_id = $int64:product$ ;
		planet.user_id = user.id ;
		>>)
  >|= List.map (fun x -> x#!u,Int64.to_int x#!p)

let fetch_by_project project =
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

let fetch_by_project_user project user =
  (view
   << { p = planet.id ; 
		loc = planet.location ;
        prod = planet.product_id } |
    planet in $planets$ ;
    planet.user_id = $int64:user$ ;
    planet.project_id = $int64:project$
	>>)
  >|= List.map (fun x -> x#!p,x#!loc,x#?prod)


(** Give the number of free planets for each user in a given project *)
let fetch_free_by_user project_id = 
  (view
		  << group { id = u_id ; name = u_name ; planets = count[planet.id] } by {u_id = planet.user_id ; u_name = user.name } |
			planet in $planets$ ;
			user in $users$ ;
			user.id = planet.user_id ;
			planet.project_id = $int64:project_id$ ;
			is_null planet.product_id ;
			>>)
  >|= List.map (fun x -> x#!id, x#!name, x#!planets)

end



(** {1 Users} *)
(**************)

module QUser = struct

let exist name =
  (view
   << {password = user.password} |
    user in $users$;
    user.name = $string:name$ >>)
  >|= (function [] -> false | _ -> true)
	 
let get_id name = 
  (view_one
   << {id = user.id } |
    user in $users$ ;
    user.name = $string:name$ 
	>>)
  >|= (fun p -> p#!id)

exception User_allready_exists

let create name pwd =
  lwt b = exist name in
  if not b then
	let h = Bcrypt.string_of_hash (Bcrypt.hash pwd) in
	query
	  <:insert< $users$ :=
			   { id = users?id ;
			   name = $string:name$ ; 
			   password = $string:h$  } >>
  else raise_lwt User_allready_exists

(* TODO : make password secure *)
let check_pwd name pwd =
  (view_one
	 <:view< {p = user.password} |
			user in $users$;
			user.name = $string:name$ 
  >>)
  >|= (fun s -> Bcrypt.verify pwd (Bcrypt.hash_of_string s#!p))

let attach project user =
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

end


(** {1 Admin} *)
(**************)

module QAdmin = struct 
let promote project user =
  query
	<:insert< $projects_admins$ :=
			 { project_id = $int64:project$;
			 user_id = $int64:user$ ;
			 } >>

let verify project user = 
  (query
	 <:select< row |
			  row in $projects_admins$ ;
			  row.user_id = $int64:user$ ;
			  row.project_id = $int64:project$ ;
			  >>)
  >|= (function [] -> false | _ -> true)
end

(** {1 Helpers} *)

(** Trick module to be able to cast macaque object whithout the syntax extension, usefull in .eliom files ... *)
module Helpers = struct

let id p = p#!id 

let proj p = p#!proj
let proj_opt p = p#?proj

let loc p = p#!loc

let prod p = p#!prod
let prod_opt p = p#?prod

let note p = p#!note
let note_opt p = p#?note

end



