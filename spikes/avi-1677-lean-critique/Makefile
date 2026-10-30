LEAN_PREFIX ?= /nix/store/cwilxazl2n8f59lksc4xihx560485ci8-lean4-4.29.1
LEAN  := $(LEAN_PREFIX)/bin/lean
LEANC := $(LEAN_PREFIX)/bin/leanc
LEAN_LDFLAGS := $(shell $(LEANC) --print-ldflags)

all: consumer

Critique.c: Critique.lean
	$(LEAN) -c $@ Critique.lean

Critique.o: Critique.c
	$(LEANC) -c $< -o $@

consumer: Consumer.hs Critique.o
	ghc -O2 Consumer.hs Critique.o -o consumer $(LEAN_LDFLAGS)

run: consumer
	./consumer

clean:
	rm -f Critique.c Critique.o Consumer.hi Consumer.o consumer
