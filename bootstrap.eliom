{shared{
open Eliom_lib
open Eliom_content
open Eliom_content.Html5
open Eliom_content.Html5.F
}}

(** Faire une page en bonne et due forme *)
let make_page ?(a=[]) ?(css=[]) ?(js=[]) s bodyl = 
  Eliom_tools.F.html
    ~title:s
    ~a:a
    ~css:(
      ["css";"bootstrap.min.css"] ::
        ["css";"bootstrap-responsive.min.css"] ::
        css
    )
    ~js:(
	  ["js";"jquery.js"] :: 
		["js";"jquery.tools.min.js"] :: 
		["js";"bootstrap.min.js"] :: js)
    (body bodyl)


{shared{
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

let dummy_a ?(a=[]) content = 
  Raw.a ~a:((a_class ["link"]) :: a) content

(** Des éléments bootstrap *)

(** Icons *)
let icon ?(a=[]) ?(white=false) s = 
  let style = if white then ["icon-white"] else [] in 
  i ~a:(a_class (("icon-"^s) :: style) :: a) []

let d_icon ?(a=[]) ?(white=false) s = 
  let style = if white then ["icon-white"] else [] in 
  i ~a:(a_class (("icon-"^s) :: style) :: a) []

(** Caret *)
let caret = 
  spanc "caret" []

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

}}

(** Les menus *)

(* Permet de savoir si un service correspond a l'url courante *)
let same_service_opt ~current s =
  let same_url url =
    make_string_uri ~absolute_path:true ~service:s () = url in
  match current with
    | None ->
        same_url ("/"^(Eliom_request_info.get_current_sub_path_string ()))
    | Some s' -> same_url (make_string_uri ~absolute_path:true ~service:s' ())

(* Une fonction de menu, un poil overkill *)
let menu ?(prefix=[]) ?(postfix=[]) ?(active=["active"]) ?(liclasses=[]) ?(classes=[]) ?id ?service:current l () = 
  let rec aux = function
    | [] -> postfix
    | (url, text)::l ->
        (if same_service_opt ?current url
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

{shared{

(** {2 Collasping} *)
module Collapse = struct

  (** Data attribute to make an element toogle colapse another element. Take the id of the element to collapse whithout "#" *)
  let a_collapse id = 
	[a_user_data "toggle" "collapse" ; a_user_data "target" ("#"^id)]

  (** Button that toogle-collapse the element with the given id *)
  let button ?(a=[]) id content = 
	button 
      ~a:(lclasses ["btn"] :: a_collapse id @ a)
      ~button_type:`Button content

  (** Div to encapsulate the element to collapse *)
  let div ?(a=[]) id content = 
	div 
	  ~a:(a_id id :: lclasse "collapse" :: a)
	  content 

  let a ?(a=[]) id content = 
	dummy_a ~a:(a_collapse id @ a) content
end

}}

{shared{
(** {2 Dropdown Menus} *)


module Dropdown = struct
  
  let a_dropdown = a_user_data "toggle" "dropdown"

  let wrap (elem: ?a:'a -> 'b) ?(a=[]) ?(right=false) title content = 
	let menu_class = if right then ["pull-right"] else [] in 
	[elem
	   ~a:(
		 a_class ["dropdown-toggle"] ::
		 a_dropdown ::
	   a)
	   title  ;
	 ul ~a:[a_class ("dropdown-menu"::menu_class)] content
	]

  let a ?(right=false) title content = 
	wrap 
	  Raw.a 
	  ~a:[a_class ["link"]]
	  ~right title content 

  

  let nav =
	let a_ = a in (fun ?(a=[]) ?(right=false) title content ->
	li ~a:((a_class ["dropdown"]) :: a) (a_ ~right title content))

  let btn ?(a=[]) ?(right=false) title content = 
	wrap 
	  (button ~button_type:`Button) 
	  ~a:(a_class ["btn"] :: a)
	  ~right title content 

  let btngroup ?(a=[]) ?(right=false) title content = 
	divc "btn-group" (btn ~a ~right title content)

end
}}

(** {2 ToogleClass} 
	Little utility class to toogle a class on a target when fireing events on an element.
*)
{shared{
module type CLASS = sig
  val v : string
  val revert : bool
end
}}

{client{
module ToogleClass (Class : CLASS) = struct

  let up node =
    node##classList##add(Js.string Class.v)
  let down node = 
    node##classList##remove(Js.string Class.v)
  let toogle node = 
    node##classList##toogle(Js.string Class.v)

  let make_onoff event_on event_off element target =
    let on = if Class.revert then down else up in 
	let off = if Class.revert then up else down in 
	Lwt.async (fun () -> 
      event_on
        element
        (fun _ _ ->
           on target ;
           event_off element >|= (fun _ -> off target)))

  let make_toogle event_toogle element target =
	Lwt.async (fun () -> 
	  event_toogle
		element
		(fun _ _ -> toogle target))

  let onhover element target =
	make_onoff
	  Lwt_js_events.mouseovers Lwt_js_events.mouseout 
	  element target 

  let onclic element target = 
	make_toogle
	  Lwt_js_events.clicks
	  element target 
end
}}

(** {2 Typeahead}
    http://twitter.github.io/bootstrap/javascript.html#typeahead 
*)

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
    let obj = make_object (U.obj [| |]) user_data in
    let data = U.fun_call (U.variable "jQuery") [|U.inject i|] in
    ignore (U.meth_call data "typeahead" [| U.inject obj|] )


end

}}

{shared{

module Popover_html = struct

  type position = 
	  Right | Left | Top | Bottom

  let str_position = 
	let right = "right" in 
	let left = "left" in 
	let top = "top" in 
	let bottom = "bottom" in 
	function
	  | Right -> right
	  | Left -> left
	  | Top -> top
	  | Bottom -> bottom

  type trigger = 
	  Click | Hover | Focus | Manual

  let layout s position tip_title tip_content = 
	div ~a:(classes 
		(s :: (function Some x -> [str_position x] | None -> []) position))
	  [ divc "arrow" [] ;
		h3 ~a:(classe (s ^ "-title")) tip_title ;
		divc (s ^ "-content") tip_content
	  ]

end
}}


{client{
module Popover = struct

  open Js
  open Popover_html
  module U = Js.Unsafe 

  let js_position = 
	let right = string "right" in 
	let left = string "left" in 
	let top = string "top" in 
	let bottom = string "bottom" in 
	function
	  | Right -> right
	  | Left -> left
	  | Top -> top
	  | Bottom -> bottom


  let js_trigger = 
	let click = string "click" in 
	let hover = string "hover" in 
	let focus = string "focus" in 
	let manual = string "manual" in 
	function
	  | Click -> click
	  | Hover -> hover
	  | Focus -> focus
	  | Manual -> manual

  let apply
	  ?(html : bool t option)
	  ?(animation : bool t option)
	  ?placement
	  ?(selector : js_string t option)
	  ?trigger
	  ?(title : #Dom_html.element t option)
	  ?(content : #Dom_html.element t option)
	  ?(delay : float t option)
	  ?(container : js_string t option)
      (e : #Dom_html.element t)
	=
    let opt_inject x = opt_map U.inject x in
	let placement = opt_map (fun x -> U.inject (js_position x)) placement in
	let trigger =  opt_map (fun x -> U.inject (js_trigger x)) trigger in
	let title = opt_map (fun x -> U.inject x##innerHTML) title in 
	let content = opt_map (fun x -> U.inject x##innerHTML) content in 
    let user_data = 
      [ "html", opt_inject html ;
		"animation", opt_inject animation ; 
		"placement", placement ; 
		"selector", opt_inject selector ; 
		"trigger", trigger ; 
		"title", title ;  
		"content", content ; 
		"delay", opt_inject delay ; 
		"container", opt_inject container ; 
      ] in
    let rec make_object obj = function
      | [] -> obj
      | (_, None)::l -> make_object obj l
      | (s, Some v) :: l -> U.set obj s v ; make_object obj l
    in 
    let obj = make_object (U.obj [| |]) user_data in
    let data = U.fun_call (U.variable "jQuery") [|U.inject e|] in
    ignore (U.meth_call data "popover" [| U.inject obj|] )

  let show (e: #Dom_html.element t) = 
	let data = U.fun_call (U.variable "jQuery") [|U.inject e|] in 
	ignore (U.meth_call data "popover" [| U.inject (string "show")|])

  let hide (e: #Dom_html.element t) = 
	let data = U.fun_call (U.variable "jQuery") [|U.inject e|] in 
	ignore (U.meth_call data "popover" [| U.inject (string "hide")|])

  let toogle (e: #Dom_html.element t) = 
	let data = U.fun_call (U.variable "jQuery") [|U.inject e|] in 
	ignore (U.meth_call data "popover" [| U.inject (string "toogle")|])

  let destroy (e: #Dom_html.element t) = 
	let data = U.fun_call (U.variable "jQuery") [|U.inject e|] in 
	ignore (U.meth_call data "popover" [| U.inject (string "destroy")|])

end
}}


(** {2 jQuery tools ToolTip }
	http://jquerytools.org/documentation/tooltip/index.html *)

{shared{
module Tooltip_html = struct
  
  type pos_h = [ `Right | `Left | `Center ]
   
  type pos_v = [ `Top | `Bottom | `Center ]
  
  let str_pos_h = 
	let right = "right" in 
	let left = "left" in 
	let center = "center" in 
	function
	  | `Right -> right
	  | `Left -> left
	  | `Center -> center

  let str_pos_v = 
	let top = "top" in 
	let bottom = "bottom" in 
	let center = "center" in 
	function
	  | `Bottom -> bottom
	  | `Top -> top
	  | `Center -> center

  let str_pos (v,h) = str_pos_v v ^ " " ^ str_pos_h h

end 

}}


{client{

module Tooltip = struct

  open Js
  module U = Js.Unsafe 
	  
  open Tooltip_html

  let js_pos_h = 
	let right = string "right" in 
	let left = string "left" in 
	let center = string "center" in 
	function
	  | `Right -> right
	  | `Left -> left
	  | `Center -> center

  let js_pos_v = 
	let top = string "top" in 
	let bottom = string "bottom" in 
	let center = string "center" in 
	function
	  | `Bottom -> bottom
	  | `Top -> top
	  | `Center -> center

  let js_pos (v,h) = (js_pos_v v)##concat_2(string " ",js_pos_h h)

  (* Argument not binded yet : 
	 events, take an object 
	 layout, take html as string
  *)


  let apply
	  ?(cancelDefault : bool t option)
	  ?(effect : js_string t option)
	  ?(delay : int option)
	  ?(offset : (int * int) option)
	  ?(opacity : int option)
	  ?(position : (pos_v * pos_h) option)
	  ?(predelay : int option)
	  ?(relative : bool t option)
	  ?(tip : js_string t option)
	  ?(tipClass : js_string t option)
      e
	=
	let e = string e in 
    let opt_inject x = opt_map U.inject x in
	let position = opt_map (fun x -> U.inject (js_pos x)) position in
	let offset = opt_map (fun (x,y) -> U.inject (array [| x ; y |])) offset in
    let user_data = 
      [ "cancelDefault", opt_inject cancelDefault ;
		"effect", opt_inject effect ;
		"delay", opt_inject delay ;
		"offset", offset ;
		"opacity", opt_inject opacity ;
		"position", position ;
		"predelay", opt_inject predelay ;
		"relative", opt_inject relative;
		"tip", opt_inject tip;
		"tipClass", opt_inject tipClass;
      ] in
    let rec make_object obj = function
      | [] -> obj
      | (_, None)::l -> make_object obj l
      | (s, Some v) :: l -> U.set obj s v ; make_object obj l
    in 
    let obj = make_object (U.obj [| |]) user_data in
    let data = U.fun_call (U.variable "jQuery") [|U.inject e|] in
    ignore (U.meth_call data "tooltip" [| U.inject obj|] )
	  
  let show (e: #Dom_html.element t) = 
	Eliom_lib.debug "show !" ;
	let data = U.fun_call (U.variable "jQuery") [|U.inject e|] in 
	let api = U.meth_call data "data" [| U.inject (string "tooltip") |] in 
	ignore (U.meth_call api "show" [| |])

  let hide (e: #Dom_html.element t) = 
	Eliom_lib.debug "hide !" ;
	let data = U.fun_call (U.variable "jQuery") [|U.inject e|] in 
	let api = U.meth_call data "data" [| U.inject (string "tooltip") |] in 
	ignore (U.meth_call api "hide" [| |])



end
}}
