# BUILD FLAGS
OCB_FLAGS = -r -use-ocamlfind -pkgs 'sexplib' -I src -tag 'debug'
OCB = ocamlbuild $(OCB_FLAGS)

# RULES
all: native

clean:
	$(OCB) -clean

native:
	$(OCB) godel.native

byte:
	$(OCB) godel.byte
