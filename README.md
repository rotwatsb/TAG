# TAG
An ocaml game engine for text-based adventures

compile with "ocamlfind ocamlc -linkpkg -package yojson -package str game.ml -o game.byte"

run on game file "mch.json" with "./game.byte mch.json"

game.ml will run any Text-based Adventure Game given as a json file adhering to the format specified in schema.json

mch.json is a work in progress, although it should be playable.. The story is just now getting some serious attention. I'm hoping to expand upon it a lot in the coming weeks.
