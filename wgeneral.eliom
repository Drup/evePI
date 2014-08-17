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
		   Lwt.return (Manip.replaceChildren container replacement)
		 ))

  let multishot container replacement event trigger =
	Lwt.async
	  (fun () ->
		 event trigger (fun _ _ ->
		   Lwt.return (Manip.replaceChildren container replacement)
		 ))

  let toogle container original replacement event1 event2 trigger =
	Lwt.async (fun () ->
	  event1 trigger (fun _ _ -> (
		debug "bla" ; Manip.replaceChildren container replacement ;
		lwt _ = Lwt_js.sleep 0.1 in
		(event2 trigger  >>= (fun _ ->
		  Manip.replaceChildren container original ;
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

{shared{
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
	  let trigger, confirm, cancel = %trigger, %confirm, %cancel in
	  let fake_input = %fake_input in
	  let replacement = [fake_input; confirm; cancel] in
	  let _ = Replacer.click_multishot container replacement
		  (To_dom.of_element trigger) in
	  let _ = Replacer.click_multishot container [original;trigger]
		  (To_dom.of_element cancel) in
	  Lwt.async (fun () ->
		let open Lwt_js_events in
		click (To_dom.of_element confirm) >>= (fun _ ->
		  let new_name = Js.to_string (To_dom.of_span fake_input)##innerHTML in
		  Eliom_client.change_page ~service:%service () (%id,new_name)
		))
	}} in
  container
}}

{client{

let opt_iter f = function
  | None -> ()
  | Some x -> f x

let make_draggable ?(label=Js.string "text") dom_ref (elem,info) =
  let open Lwt_js_events in
  let data = Js.string "" in

  let ondragstart ev _ =
    dom_ref := Some (elem,info) ;
    ev##dataTransfer##setData(label, data) ;
    Lwt.return () in

  let ondragend ev _ =
    if ev##dataTransfer##dropEffect = Js.string "none" then
      dom_ref := None ;
    Lwt.return () in

  Lwt.async (fun () -> dragstarts elem ondragstart) ;
  Lwt.async (fun () -> dragends elem ondragend)


let make_dropzone dom_ref
    ?(label=Js.string "text") ?drop_callback ?over_class
    (dropzone,f,info) =
  let open Lwt_js_events in
  let ondragover  ev _   =
    if Js.to_bool ev##dataTransfer##types##contains(label) then
      begin
	Dom.preventDefault ev ;
	opt_iter (fun c -> dropzone##classList##add(c)) over_class ;
	ev##dataTransfer##dropEffect <- Js.string "move"
      end ;
    Lwt.return ()
  in

  let ondrop ev _  =
    Dom.preventDefault ev ;
    opt_iter (fun c -> dropzone##classList##add(c)) over_class ;
    let elem = !dom_ref in
    dom_ref := None ;
    opt_iter (fun (elem, draginfo) ->
      opt_iter (fun f -> f info draginfo) drop_callback ;
      Js.Opt.iter (elem##parentNode) (fun x -> Dom.removeChild x elem) ;
      f elem draginfo) elem ;
    Lwt.return () in

  let ondragleave ev _ =
    Dom.preventDefault ev ;
    opt_iter (fun c -> dropzone##classList##add(c)) over_class ;
    Lwt.return () in

  Lwt.async (fun () -> dragovers dropzone ondragover) ;
  Lwt.async (fun () -> drops dropzone ondrop) ;
  Lwt.async (fun () -> dragleaves dropzone ondragleave)

let draggable_init
    ?drop_callback ?label ?over_class
    draggables dropzones =
  Random.self_init () ;
  let label = Js.string (
      match label with
	| Some x -> x
	| None ->  Random.self_init () ; string_of_int (Random.bits ())
    ) in
  let dom_ref = ref None in
  List.iter (make_draggable ~label dom_ref) draggables ;
  List.iter (make_dropzone ~label dom_ref ?drop_callback ?over_class) dropzones

}}



(** Let's package everything into F and D modules *)

{shared{
module Packager (H : module type of D) = struct

  include H
  module STitle = STitle (H)

end

module F = Packager (F)

module D = Packager (D)
}}
