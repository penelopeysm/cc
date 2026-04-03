C compiler.

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
