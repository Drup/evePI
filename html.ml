open Eliom_lib
open Eliom_content
open Eliom_content.Html5.F

(** Faire une page en bonne et due forme *)

let make_page ?(a=[]) ?(css=[]) ?(js=[]) s bodyl = 
  Lwt.return (
		Eliom_tools.D.html
		  ~title:s
		  ~a:a
		  ~css:(
			 ["css";"bootstrap-responsive.min.css"] ::
				["css";"bootstrap.min.css"] :: css
		  )
		  ~js:( ["js";"bootstrap.min.js"] :: js)
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

(* Les labels *)

let label_default ?(c=[]) content = 
  spancs ("label"::c) content

let label_success content = label_default~c:["label-success"] content 
let label_warning content = label_default~c:["label-warning"] content 
let label_important content = label_default~c:["label-important"] content 
let label_info content = label_default~c:["label-info"] content 
let label_inverse content = label_default~c:["label-inverse"] content 

(* Les badges *)
let badge ?(c=[]) content = 
  spancs ("badge"::c) content

let badge_success content = badge ~c:["badge-success"] content 
let badge_warning content = badge ~c:["badge-warning"] content 
let badge_important content = badge ~c:["badge-important"] content 
let badge_info content = badge ~c:["badge-info"] content 
let badge_inverse content = badge ~c:["badge-inverse"] content 

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
	 divc "navbar-inner" body
  ]
      
