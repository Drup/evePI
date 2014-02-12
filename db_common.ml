module Lwt_thread = struct
  include Lwt
  include Lwt_chan
end
module Lwt_PGOCaml = PGOCaml_generic.Make(Lwt_thread)
module Lwt_Query = Query.Make_with_Db(Lwt_thread)(Lwt_PGOCaml)

let connect db_name () =
  Lwt_PGOCaml.connect
    ~database:db_name
    ()

module type CONFIG = sig
  val connect : unit -> 'a Lwt_PGOCaml.t Lwt.t
  val pool_number : int
end

module Make (Config : CONFIG) = struct

  let validate db =
    Lwt.try_bind
      (fun () -> Lwt_PGOCaml.ping db)
      (fun () -> Lwt.return true)
      (fun _ -> Lwt.return false)

  let pool : Lwt_PGOCaml.pa_pg_data Lwt_PGOCaml.t Lwt_pool.t
    = Lwt_pool.create Config.pool_number ~validate Config.connect

  let exec f ?log x = Lwt_pool.use pool (fun db -> f db ?log x)

  let view ?log x = exec Lwt_Query.view ?log x
  let view_opt ?log x = exec Lwt_Query.view_opt ?log x
  let view_one ?log x = exec Lwt_Query.view_one ?log x
  let query ?log x = exec Lwt_Query.query ?log x
  let value ?log x = exec Lwt_Query.value ?log x
  let value_opt ?log x = exec Lwt_Query.value_opt ?log x

end

module Config_sdd = struct
  let connect () = connect "eveSDD" ()
  let pool_number = 10
end

module Config_pi = struct
  let connect () = connect "evePI" ()
  let pool_number = 1 (* We must keep only one connexion because we use serials *)
end


let () = Lwt_PGOCaml.verbose := 2

module SDD = Make(Config_sdd)
module PI = Make(Config_pi)
