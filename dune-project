(lang dune 2.8)

(name sushibot)

(generate_opam_files true)

(package
 (name sushibot)
 (depends cohttp-lwt-unix yojson caqti-lwt caqti-driver-sqlite3 lwt_ssl))
