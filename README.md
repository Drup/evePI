Eveπ
=====
Eveπ is a tool to organize PI projects involving several peoples on Eve Online.
For now, it's in *very early alpha*, not very well structured and probably unstable.


* * * * *

The tool is written in [Eliom][eliom], a language/framework to program websites in Ocaml.
I use [Macaque][macaque] for pgsql interfacing in a type-safe way.
I also use [Bootstrap][bootstrap] for the website layout.

You will also need [sqlite][sqlite], [safepass][safepass] and [macaque_lwt][macaque_lwt].

Install all of this easily with opam :

```
opam install sqlite3-ocaml safepass macaque eliom macaque_lwt
```

How to install it
----
For now, you need to have two pgsql databases :
- One named eveSDD containing the [SDD](http://wiki.eve-id.net/CCP_Static_Data_Dump);
- One named evePI with the schema [schema.sql](schema.sql).

Those should be accessible in local. You can then use ocsigen as described in the [Readme](README).

No, it's not very flexible, it will change ...


Participate
----
Sure, see the [todo file](TODO.md). You can of course propose features (or do them even !)
I welcome ~css masters~ and designers very warmly too, because I suck at it.

Don't be afraid by french comments in the code ... It happens, sometimes.

See [Hacking.md](Hacking.md) for a description of the code organization.


[eliom]: http://ocsigen.org/eliom/ "Eliom"
[bootstrap]: http://twitter.github.io/bootstrap/ "Bootstrap"
[macaque]: http://macaque.forge.ocamlcore.org/ "Macaque"
[macaque_lwt]: https://bitbucket.org/deplai_j/macaque_lwt "Macaque_lwt"
[sqlite]: https://bitbucket.org/mmottl/sqlite3-ocaml "Sqlite3-ocaml"
[safepass]: http://ocaml-safepass.forge.ocamlcore.org/ "Safepass"
