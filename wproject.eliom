(** This module defines every Project-related widgets *)

{shared{
open Eliom_lib
open Eliom_content.Html5
}}

open Skeleton
open Auth
open EvePI_db
open Utility

open F


(** {3 Join a project} *)

let join_service = 
  Eliom_service.post_coservice'
    ~post_params:Eliom_parameter.(int64 "project") ()

let join_btn project_id =
  button
    ~button_type:`Button
    ~a:[a_class ["btn"] ; 
        a_onclick 
          {{ fun _ -> 
              ignore (Eliom_client.change_page  
                  ~service:%join_service () %project_id) }}]
    [pcdata "Join now !"]

let _ =
  Wrap.action_register
    ~service:join_service
    (fun user () project -> (
         lwt _ = QUser.attach project user.id in
         Lwt.return ()
       ))

(** Project list *)
let make_list user = 
  lwt projects = QProject.fetch_all () 
  and my_projects = QProject.fetch_by_user user in
  let make_button id name =
    if List.exists (fun (i,_) -> i = id) my_projects 
    then button 
        ~a:[a_class ["btn"; "disabled"]]
        ~button_type:`Button [pcdata "Already in !"]
    else join_btn id
  in
  let aux (id, name, desc) =
    ((dt [span ~a:[a_class ["btn-group"]] [
         a ~a:[a_class ["btn"]]
	   ~service:project_member_coservice 
	   [pcdata name] id ; 
         make_button id name
       ]],[]),
     (dd [pcdata desc],[]))
  in
  Lwt.return (List.map aux projects)


(** {3 Create a project} *)

let create_service =
  Eliom_service.post_coservice
    ~fallback:project_list_service
    ~post_params:Eliom_parameter.(
	string "name" ** 
	  string "description" ** 
	  int32 "goal") ()

let create_form () = 
  lwt goals = Sdd.get_possible_goals () in
  let (ghd,gtl) = 
    let f (id,name) = 
      Option ([],id,Some (pcdata name),true) in
    f (List.hd goals), List.map f (List.tl goals)
  in
  let fun_form (name,(desc,goal)) = 
    [div ~a:[a_class ["input-prepend";"input-append"]]
       [ string_input ~a:[a_placeholder "Name"] 
	   ~input_type:`Text ~name:name () ;
	 string_input ~a:[a_placeholder "Description"] 
	   ~input_type:`Text ~name:desc () ;
	 int32_select ~name:goal ghd gtl ;
	 button 
	   ~a:[a_class ["btn"]]
	   ~button_type:`Submit [pcdata "Create"] ;
       ]] in 
  Lwt.return (
    post_form ~a:[a_class ["form-inline"]]
      ~service:create_service
      fun_form ())

let _ = 
  Wrap.action_with_redir_register 
    ~redir:project_list_service
    ~service:create_service
    (fun admin () (project,(desc,goal)) -> (
         lwt project_id = QProject.create project desc in
         lwt _ = QUser.attach project_id admin.id in
         lwt _ = QAdmin.promote project_id admin.id in
         lwt tree = 
           Tree.make (fun id -> (Sdd.get_sons id) >|= List.map fst) goal in 
         lwt _ = QProject.fill_tree project_id tree in
         Lwt.return ()
       ))


(** {2 Project adminstration widgets *)

(** {3 Add a new goal to a project} *)

let add_goal_service = 
  Eliom_service.post_coservice'
    ~post_params:Eliom_parameter.(int64 "project" ** int32 "goal") ()

let add_goal_form project_id = 
  lwt goals = Sdd.get_possible_goals () in
  let (ghd,gtl) = 
    let f (id,name) = 
      Option ([],id,Some (pcdata name),true) in
    f (List.hd goals), List.map f (List.tl goals)
  in
  let form_fun (project_form,goal_form) = 
    [ div ~a:[a_class ["input-append"]]
	[ int32_select ~name:goal_form ghd gtl ;
	  int64_button 
	    ~a:[a_class ["btn"]]
	    ~name:project_form
	    ~value:project_id
	    [pcdata "Add"] 
	]]
  in 
  Lwt.return (
    post_form 
      ~a:[a_class ["form-inline"]]
      ~service:add_goal_service
      form_fun ()
  )

let _ = 
  Wrap.action_with_redir_register 
    ~service:add_goal_service
    (fun admin () (project_id,goal) -> 
       lwt is_admin = QAdmin.verify project_id admin.id in
       if is_admin then
         lwt tree = 
	   Tree.make (fun id -> (Sdd.get_sons id) >|= List.map fst) goal in 
         lwt _ = QProject.fill_tree project_id tree in
         Lwt.return ()
       else 
	 Lwt.return ()
    )

(** {3 Change the name of a project} *)

let change_name_service = 
  Eliom_service.post_coservice'
    ~post_params:Eliom_parameter.(int64 "project" ** string "name") ()

let _ = 
  Wrap.action_with_redir_register 
    ~service:change_name_service
    (fun admin () (project_id,name) -> 
       lwt is_admin = QAdmin.verify project_id admin.id in
       if is_admin && name <> "" then
         lwt _ = QProject.update_name project_id name in
         Lwt.return ()
       else 
	 Lwt.return ()
    )
