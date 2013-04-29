open Eliom_lib
open Eliom_content
open Eliom_service

module EvePI_app =
  Eliom_registration.App (
  struct
    let application_name = "Eveπ"
  end)

let main_service =
  Eliom_service.service
    ~path:[""] ~get_params:Eliom_parameter.unit ()
