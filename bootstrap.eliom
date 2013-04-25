{shared{
open Eliom_lib
open Eliom_content
open Eliom_content.Html5.F
}}
(** Faire une page en bonne et due forme *)
 
let make_page ?(a=[]) ?(css=[]) ?(js=[]) s bodyl = 
  Lwt.return (
	 Eliom_tools.D.html
		~title:s
		~a:a
		~css:(
		  ["css";"bootstrap.min.css"] ::
			 ["css";"bootstrap-responsive.min.css"] ::
			 css
		)
		~js:(["js";"jquery.js"] :: ["js";"bootstrap.min.js"] :: js)
		(body bodyl)
  )

(** Quelques utilitaires generiques *)

let lclasse s = a_class [s]
let lclasses s = a_class s

let classe s = [a_class [s]]
let classes s = [a_class s]

let divc s l =
  div ~a:(classe s) l

let divcs s l =
  div ~a:(classes s) l

let spanc s l =
  span ~a:(classe s) l

let spancs s l =
  span ~a:(classes s) l

let center l = 
  div ~a:(classe "text-center") l


(** Des éléments bootstrap *)

(** Les labels *)
module Label = struct

  let default ?(c=[]) content = 
	 spancs ("label"::c) content

  let success content = default~c:["label-success"] content 
  let warning content = default~c:["label-warning"] content 
  let important content = default~c:["label-important"] content 
  let info content = default~c:["label-info"] content 
  let inverse content = default~c:["label-inverse"] content 

end

(** Les badges *)
module Badge = struct 

  let default ?(c=[]) content = 
	 spancs ("badge"::c) content

  let success content = default ~c:["badge-success"] content 
  let warning content = default ~c:["badge-warning"] content 
  let important content = default ~c:["badge-important"] content 
  let info content = default ~c:["badge-info"] content 
  let inverse content = default ~c:["badge-inverse"] content 

end

(** Le layout grille de bootstrap *)

let grid_content ?(postfix="") columns =
  let span i = Printf.sprintf "span%i" i in
  let aux (size,content) = divc (span size) content in
  let body = List.map aux columns in
  divc ("row"^postfix) body

let grid ?(postfix="") ?(head = []) ?size columns =
  let t = match size with 
	 | None -> List.fold_left (fun s (x,_) -> s + x) 0 columns
	 | Some i -> i
  in
  divc ("row"^postfix) [ 
    divc (Printf.sprintf "span%i" t) (
	   head @
		[grid_content ~postfix columns]
		 )]
				
let container ?(postfix="") ?(head = []) columns =
  divc ("container"^postfix) ( 
		head @
      [grid_content ~postfix columns]
		 )

let grid_fluid ?(head=[]) = grid ~postfix:"-fluid" ~size:12 ~head

let container_fluid ?(head=[]) = container ~postfix:"-fluid" ~head

(** Les menus *)

(* Permet de savoir si un service correspond a l'url courante *)
let same_service_opt s sopt =
  let same_url url =
	 make_string_uri ~absolute_path:true ~service:s () = url in
  match sopt with
  | None ->
	  same_url ("/"^(Eliom_request_info.get_current_sub_path_string ()))
  | Some s' -> same_url (make_string_uri ~absolute_path:true ~service:s' ())

(* Une fonction de menu, un poil overkill *)
let menu ?(prefix=[]) ?(postfix=[]) ?(active=["active"]) ?(liclasses=[]) ?(classes=[]) ?id ?service:current l () = 
  let rec aux = function
	 | [] -> postfix
	 | (url, text)::l ->
		 (if same_service_opt url current
		  then (li ~a:[a_class (active@liclasses)] [a url text ()])
		  else (li ~a:[a_class liclasses] [a url text ()])) :: (aux l)
  in 
  let a_ul = match id with 
	 | Some id -> [a_id id; a_class classes] 
	 | None -> [a_class classes] 
  in
  ul ~a:a_ul (prefix @ aux l)

(* Permet de wrap une navbar *)
let navbar ?(classes=[]) ?head ?(head_classes=[]) menu =
  let body = match head with 
	 | Some h -> (divcs ("brand"::head_classes) h) :: menu
	 | None -> menu
  in
  divcs ("navbar"::classes) [
	 divc "navbar-inner" [
		divc "container" body
	 ]]

(** Typeahead
  http://twitter.github.io/bootstrap/javascript.html#typeahead *)


{shared{
let rec add_user_data aux = function 
  | [] -> aux 
  | (s,None) :: l -> add_user_data aux l 
  | (s,Some x) :: l -> add_user_data 
		(a_user_data s x :: aux) 
		l
  
let opt_map f = function Some x -> Some (f x) | None -> None 
}}

{client{
module Typeahead = struct

  open Js
  module U = Js.Unsafe 

  let apply
		(* The method used to determine if a query matches an item. *)
		?(matcher : (js_string t -> bool t) option)
		(* Method used to sort autocomplete results. *)
		?(sorter : (js_string t js_array t -> js_string t js_array t) option)
		(* Method used to highlight autocomplete results. *)
		?(highlighter : (js_string t -> #Dom_html.element t) option)
		(* The method used to return selected item. *)
		?(updater : (js_string t -> js_string t) option)
		(* The data source to query against. *)
		?(source : js_string t js_array t option)
		(* The max number of items to display in the dropdown. default : 8 *)
		?(items : int option)
		(* The dropdown menu. default : <ul class="typeahead dropdown-menu"></ul> *)
		?(menu : #Dom_html.element option)
		(* A dropdown item. default : <li><a href="#"></a></li> *)
		?(item : #Dom_html.element option)
		(* The minimum character length needed before triggering autocomplete suggestions. default : 1 *)
		?(minLength : int option)
		(* The input object *)
		(i : #Dom_html.inputElement t)
	 =
	 let opt_inject x = opt_map U.inject x in
	 let user_data = 
		[ "matcher", opt_inject matcher ; 
		  "sorter", opt_inject sorter ;
		  "highlighter", opt_inject highlighter ;
		  "updater", opt_inject updater ;
		  "source", opt_inject source ;
		  "items", opt_inject items ; 
		  "menu", opt_inject menu ;
		  "item", opt_inject item ;
		  "minLength", opt_inject minLength ;
		] in
	 let rec make_object obj = function
		| [] -> obj
		| (_, None)::l -> make_object obj l
		| (s, Some v) :: l -> U.set obj s v ; make_object obj l
	 in 
	 try 
		let obj = make_object (U.obj [| |]) user_data in
		let data = U.fun_call (U.variable "$") [|U.inject i|] in
		ignore (U.meth_call data "typeahead" [| U.inject obj|] )
	 with exn -> debug_exn "" exn ; raise exn 


end

}}

(* TODO implement the various hook functions *)
let typeahead
	 ?a ?items ?minLength 
	 ?matcher ?sorter ?updater ?highligter source =
  let to_string = opt_map (Printf.sprintf "%i") in
  let source = "[ \"" ^ String.concat "\",\"" source ^ "\"]" in
  let user_data = 
	 [ "items", to_string items ; 
		"minLength", to_string minLength ;
	 ] in
  let a = 
	 a_class ["typeahead"] ::
		a_user_data "provide" "typeahead" ::
		a_user_data "source" source ::
		a_input_type `Text ::
		match a with None -> [] | Some l -> l in
  let a = add_user_data a user_data in
  Raw.input ~a ()

