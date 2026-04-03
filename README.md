C compiler compiling C.

# Why the name?

I tried to call it `cc`, but that led to a ton of clashes with the system's existing `cc`.

# Setup

You'll need a working installation of opam (Homebrew works for this).

```bash
git clone --recurse-submodules git@github.com:penelopeysm/cc.git
```

```bash
opam install . --deps-only
eval $(opam env)
dune build
```
