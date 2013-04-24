open Eliom_lib
open Eliom_content
open Eve_db.SDD

let products = 
  <:table< planetSchematics (
	schematicID smallint NOT NULL,
	schematicName text,
	cycleTime integer
	) >>
	 
let product_graph = 
  <:table< planetSchematicsTypeMap (
	schematicID smallint NOT NULL,
	typeID integer NOT NULL,
	quantity smallint,
	isInput integer
	) >>
	 
let invtypes = 
  <:table< invTypes (
	typeID integer NOT NULL,
	groupID integer,
	typeName text,
	description text,
	mass double,
	volume double,
	capacity double,
	portionSize integer,
	raceID integer,
	basePrice double,
	published integer,
	marketGroupID integer,
	chanceOfDuplicating double
	) >>

let invGroups =
  <:table< invGroups (
	groupID integer NOT NULL,
	categoryID integer ,
	groupName text ,
	description text,
	iconID integer ,
	useBasePrice integer ,
	allowManufacture integer ,
	allowRecycler integer ,
	anchored integer ,
	anchorable integer ,
	fittableNonSingleton integer ,
	published integer
	) >>

let mapDenormalize = 
  <:table< mapDenormalize (
  itemID integer NOT NULL,
  typeID integer ,
  groupID integer ,
  solarSystemID integer ,
  constellationID integer ,
  regionID integer ,
  orbitID integer ,
  radius double  ,
  itemName text ,
  security double  ,
  celestialIndex integer ,
  orbitIndex integer
) >>

let mapSolarSystems = 
  <:table< mapSolarSystems (
  regionID integer ,
  constellationID integer ,
  solarSystemID integer NOT NULL,
  solarSystemName text ,
  international integer ,
  regional integer ,
  constellation integer ,
  security double  ,
  factionID integer ,
  radius double  ,
  sunTypeID integer ,
  securityClass text
)>>

let opt_string = function
	 Some s -> s
  | None -> "Unnamed"

let get_name pID = 
  let name = 
	 <:view< {name = object.typeName } | 
	  object in $invtypes$ ;
	  object.typeID = $int32:pID$ ;
	  >>
  in 
  (view_one ~log:stderr name)
  >|= (fun x -> opt_string x#?name)
	 
let get_sons pID =
  let schematics = 
	 <:view< { sid = product.schematicID } |
	  product in $product_graph$ ;
	  product.isInput = $int32:Int32.zero$ ;
	  product.typeID = $int32:pID$ ;
	  >> in 
  let sons = 
	 <:view< { id = product.typeID ; name = object.typeName } |
	  product in $product_graph$ ;
	  schem in $schematics$ ;
	  product.isInput <> $int32:Int32.zero$ ;
	  product.schematicID = schem.sid ;
	  object in $invtypes$ ;
	  object.typeID = product.typeID ;
	  >> in
  (view ~log:stderr sons)
  >|= List.map (fun x -> (x#!id, opt_string x#?name))
  
let categorygoals = 43l

let get_possible_goals () = 
  let groups =
	 <:view< { id = g.groupID } |
	  g in $invGroups$ ;
	  g.categoryID = $int32:categorygoals$ ;
	  >> in
  let goals =
	 <:view< { id = pi.typeID ; name = pi.typeName } | 
	  g in $groups$ ;
	  pi in $invtypes$ ;
	  pi.groupID = nullable g.id ;
	  >> in
  (view ~log:stderr goals)
  >|= List.map (fun x -> (x#!id, opt_string x#?name))

let planets_id =
  [ (30889l, "Shattered");
	 (2063l, "Plasma");
	 (2017l, "Storm");
	 (2016l, "Barren");
	 (2015l, "Lava");
	 (2014l, "Oceanic");
	 (13l, "Gas");
	 (12l, "Ice");
	 (11l, "Temperate") ]

(* kinda hacky, treat planets whitout a type as shattered *)
let opt_planet_type = function
	 Some i -> i 
  | None -> 30889l

let planet_to_id t = fst (List.find (fun (_,t') -> t = t') planets_id)
let id_to_planet id = List.assoc id planets_id

let get_systems () =
  (query 
	  <:select< system | system in $mapSolarSystems$ ; >> )
  >|= List.map (fun s -> s#!solarSystemID, opt_string s#?solarSystemName)

let planet_groupID = 7l

let get_planets_by_system system_name = 
  (view 
	  << { id = planet.itemID ; name = planet.itemName ; typ = planet.typeID }
		order by planet.itemID asc |
		planet in $mapDenormalize$ ;
		system in $mapSolarSystems$ ;
		system.solarSystemName = nullable $string:system_name$ ;
		nullable system.solarSystemID = planet.solarSystemID ;
		planet.groupID = $int32:planet_groupID$ ;
		>>)
  >|= List.map (fun s -> s#!id, opt_string s#?name, opt_planet_type s#?typ)

let get_info planet_id = 
  (view_one
	  << { name = planet.itemName ; 
		  typ = planet.typeID ; 
		  system = system.solarSystemName ; } |
		planet in $mapDenormalize$ ;
		system in $mapSolarSystems$ ;
		nullable system.solarSystemID = planet.solarSystemID ;
		planet.itemID = $int32:planet_id$ ;
		>>)
	 >|= (fun s -> opt_string s#?name, id_to_planet (opt_planet_type s#?typ), opt_string s#?system)
