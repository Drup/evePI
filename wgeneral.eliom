{shared{
open Eliom_lib
open Eliom_content.Html5
open F
}}


(** {1 Generals widgets} *)
(** Regroup some widgets not specific to evePI *)


(** {2 section-title construction} *)

{shared{
module STitle (H : module type of D) = 
struct
  
  open H

  let divider () = li ~a:[a_class ["divider"]] []

  let simple content = 
	ul ~a:[a_class ["section-title"]] content

  let list content = 
	simple (List.map li content)

  let list_divider content =
	let aux = function
	  | [] -> divider ()
	  | l -> li l in
	simple (List.map aux content)

  let create ?(divider=false) =
	  if divider then list_divider
	  else list

  let one elem content = 
	list [elem content]

end
}}


(** {2 HoverGroup}
	Do stuff when hovering/clicking elements in a group *)

{shared{
module type HoverActions = sig
  type t 
  val on_hover : t option ref -> t -> unit Lwt.t
  val on_leave : t option ref -> t -> unit Lwt.t
  val on_click : t option ref -> t -> unit Lwt.t
  val on_dummy : t option ref -> unit Lwt.t
end
}}

{client{

let hovergroup_get_init 
	(type t') 
	(module G : HoverActions with type t = t' ) 
  : Dom_html.eventTarget Js.t -> t' option -> unit =
  let open Lwt_js_events in
  let current_target = ref None in
  let handle_hover trigger target =
	Lwt.async
	  (fun () -> 
		 mouseovers
		   trigger
		   (fun _ _ ->
			  lwt _ = G.on_hover current_target target in
			  mouseout trigger >>= 
			  (fun _ -> G.on_leave current_target target)))
  in
  let handle_click trigger target = 
	Lwt.async 
	  (fun () -> 
		 clicks
		   trigger
		   (fun _ _ -> G.on_click current_target target))
  in 
  let handle_dummy trigger = 
	Lwt.async 
	  (fun () -> 
		 clicks
		   trigger
		   (fun _ _ -> G.on_dummy current_target))
  in 
  let init_trigger trigger = function
	| Some elem -> 
		handle_hover trigger elem ; handle_click trigger elem
	| None -> handle_dummy trigger
  in 
  init_trigger

}}

(** {2 Replacer} 
	Replace A by B when C *)

{client{
module Replacer = struct

  open Lwt_js_events

  let oneshot container replacement event trigger = 
	Lwt.async 
	  (fun () -> 
		 event trigger >>= (fun _ -> 
		   Lwt.return (Manip.replaceAllChild container replacement)
		 ))

  let multishot container replacement event trigger = 
	Lwt.async 
	  (fun () -> 
		 event trigger (fun _ _ -> 
		   Lwt.return (Manip.replaceAllChild container replacement)
		 ))

  let toogle container original replacement event1 event2 trigger =
	Lwt.async (fun () -> 
	  event1 trigger (fun _ _ -> (
		debug "bla" ; Manip.replaceAllChild container replacement ;
		lwt _ = Lwt_js.sleep 0.1 in 
		(event2 trigger  >>= (fun _ -> 
		  Manip.replaceAllChild container original ;
		  Lwt.return ())))
	  ))

   let click_oneshot container replacement trigger = 
	 oneshot container replacement click trigger

   let click_multishot container replacement trigger = 
	 multishot container replacement clicks trigger

   let click_toogle container original replacement trigger = 
	 toogle container original replacement clicks click trigger	 

end
}}


(* TODO bind the enter key to "send" *)
let editable_name ?(default_name="New name") content service id = 
  let fake_input = 
	(* FIXME a_contenteditable is bugged in Tyxml, fix this when this is resolved !!!!! *)
	(* D.span ~a:[a_contenteditable `True] *)
	D.span ~a:[to_attrib (Xml.string_attrib "contenteditable" "true") ]
	  [pcdata default_name] in
  let btn_icon title icon_name = 
	D.Raw.a ~a:[ a_title title ; a_class ["link"] ]
	  [Bootstrap.icon ~white:true icon_name] in 
  let trigger = btn_icon "Edit name" "pencil" in 
  let confirm = btn_icon "Change name" "ok" in 
  let cancel = btn_icon "Cancel" "remove" in 
  let container = D.span ~a:[a_class ["name-input"]] [ content ; trigger ] in
  let _  = {unit{
	  let container = %container in 
	  let original = %content in 
	  let cancel = %cancel in 
	  let confirm = %confirm in 
	  let fake_input = %fake_input in 
	  let trigger = %trigger in
	  let replacement = [fake_input; confirm; cancel] in 
	  let _ = Replacer.click_multishot container replacement
		  (To_dom.of_a trigger) in
	  let _ = Replacer.click_multishot container [original;trigger]
		  (To_dom.of_a cancel) in 
	  Lwt.async (fun () -> 
		let open Lwt_js_events in 
		click (To_dom.of_a confirm) >>= (fun _ ->
		  let new_name = Js.to_string (To_dom.of_span fake_input)##innerHTML in 
		  Eliom_client.change_page ~service:%service () (%id,new_name)
		))
	}} in 
  container 


(** Let's package everything into F and D modules *)

{shared{
module Packager (H : module type of D) = struct

  include H
  module STitle = STitle (H) 
	  
end

module F = Packager (F)

module D = Packager (D)
}}
