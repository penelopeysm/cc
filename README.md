C compiler compiling C (following Nora Sandler's *Writing a C Compiler*).

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

# Extra stuff

I'm trying to take notes as I go along. These will be stored in [`NOTES.md`](./NOTES.md).
