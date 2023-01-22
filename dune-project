(lang dune 2.9)
;;< goto switch?
(generate_opam_files true)

(name mtag)
; goto 
;(source (github rand00)) 
(license GPLv3)
(authors "rand")
(maintainers "rand@r7p5.earth")

(package
 (name mtag)
 (synopsis "CLI tool to tag files in static file-hierarchies and query them")
 ;;> goto check if works with newline
 (description "File tagging is implemented via a directory of symlinks at \
	           root of static file-hierarchy")
)

