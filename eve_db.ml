open Eliom_lib

module Lwt_fonct = struct
  include Lwt
  include Lwt_chan
end
module Lwt_PGOCaml = PGOCaml_generic.Make(Lwt_fonct)
module Lwt_Query = Query.Make_with_Db(Lwt_fonct)(Lwt_PGOCaml)

let () = Lwt_PGOCaml.verbose := 2

type db = PI | SDD

let connect db_name () : unit Lwt_PGOCaml.t Lwt.t =
  Lwt_PGOCaml.connect 
    ~database:db_name
	 ()
	 

let connect_sdd () = connect "eveSDD" ()
let connect_pi () = connect "evePI" ()

module type DB =
  sig
	 val connect : unit -> unit Lwt_PGOCaml.t Lwt.t
  end
				  

module Make (Db : DB) = 
  
  struct
	 
	 let validate db =
		try
		  lwt () = Lwt_PGOCaml.ping db in
		  Lwt.return true
		with _ ->
		  Lwt.return false
						 
	 let pool = Lwt_pool.create 1 ~validate Db.connect

	 let transaction_block db f =
		Lwt_PGOCaml.begin_work db >>= fun _ ->
		try_lwt
    (* DEBUG print_endline "SQL transaction"; *)
    lwt r = f () in
		  lwt () = Lwt_PGOCaml.commit db in
		  Lwt.return r
		  with e ->
			 lwt () = Lwt_PGOCaml.rollback db in
			 Lwt.fail e
						 
	 let full_transaction_block f =
		Lwt_pool.use pool (fun db -> transaction_block db (fun () -> f db))
						 
	 let exec f x = Lwt_pool.use pool (fun db -> f db x)

	 let view ?log x = exec (fun db x -> Lwt_Query.view ?log db x) x 
	 let view_one ?log x = exec (fun db x -> Lwt_Query.view_one ?log db x) x
	 let view_opt ?log dbs x = exec (fun db x -> Lwt_Query.view_opt ?log db x) x
	 let query ?log x = exec (fun db x -> Lwt_Query.query ?log db x) x
	 let value ?log x = exec (fun db x -> Lwt_Query.value ?log db x) x

  end

module PI = Make( struct let connect = connect_pi end)
module SDD = Make( struct let connect = connect_sdd end)
