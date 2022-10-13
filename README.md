Candid for Haskell
==================

This repository contains a Haskell library for [Candid] to Haskell. You can
find the documentation for the library on
<https://nomeata.github.io/haskell-candid/Codec-Candid.html>.

The focus here is correctness with regard to the spec, and maybe also
convenience (e.g. it can guess the textual form of field names, which are just
numbers on the wires), but not necessarily high performance. Let me know if you run into troubles because of this.

This package also comes with a `hcandid` command line tool, to convert between binary and textual candid. Just run `hcandid --help` to see how to use it.

[Candid]: https://github.com/dfinity/candid/blob/master/IDL.md
