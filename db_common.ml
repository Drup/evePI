
open Macaque_lwt

let () = Lwt_PGOCaml.verbose := 2

let connect db_name () =
  Lwt_PGOCaml.connect 
    ~database:db_name
    ()


module Config_sdd = struct
  let connect () = connect "eveSDD" ()
  let pool_number = 10
end

module Config_pi = struct
  let connect () = connect "evePI" ()
  let pool_number = 1 (* We must keep only one connexion because we use serials *)
end

module SDD = Make(Config_sdd)
module PI = Make(Config_pi)
