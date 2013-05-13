{shared{
open Eliom_lib
open Eliom_content
open Eliom_service
open Eliom_content.Html5
}}

open Utility

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
module HoverGroup (G : HoverActions) = struct 
  
  open Lwt_js_events
  
  let get_init () =
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

end
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
