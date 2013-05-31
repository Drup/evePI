Agnostics libraries :
* [utility.ml](utility.ml) contains some useful stuff for the whole project.
* [tree.ml](tree.ml) for simple tree manipulations.

Eliom libraries :
* [auth.ml](auth.ml) is a module to manage authentication.
* [bootstrap.ml](bootstrap.ml) is an experimental binding for some bootstrap's widgets. It will probably be released as an external library at some point.

Db related stuff :
* [db_common.ml](db_common.ml) is the basic layer for db access.
* [sdd.ml](sdd.ml) allows to access the SDD.
* [evePI_db.ml](evePI_db.m) allows to access the evePI database.
* [qtree.ml](qtree.ml) is an extension of [tree.ml]() for project trees.

Widgets :
* [wgeneral.ml](wgeneral.ml) contains some general widgets. Those may move to an external widget library.
* [wproject.ml](wproject.ml) contains project-related widgets.
* [widget.ml](widget.ml) contains other widgets.

Main stuff :
* [skeleton.ml](skeleton.ml) defines the main services of the application and some app-wide things (like the Connected module)
* [evePI.eliom](evePI.eliom) contains all the main pages. It may contains some widgets that should be somewhere else.
