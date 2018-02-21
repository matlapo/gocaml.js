#
# Pure OCaml, package from Opam, two directories
#

# - The -I flag introduces sub-directories
# - -use-ocamlfind is required to find packages (from Opam)
# - _tags file introduces packages, bin_annot flag for tool chain
# - using *.mll and *.mly are handled automatically

# - we are using menhir, the modern replacement for OCamlYacc
# OCB_FLAGS = -use-ocamlfind             -I src -I lib # uses ocamlyacc
.PHONY: 	all clean byte native profile debug sanity test

OCB_FLAGS   = -use-ocamlfind -use-menhir
OCB = ocamlbuild $(OCB_FLAGS)

all: native # profile debug

clean:
	$(OCB) -clean

native:
	$(OCB) comp.native 


