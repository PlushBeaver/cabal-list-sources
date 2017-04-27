# `cabal-list-sources`

**Work in progress!**

Lists source and data files referenced from a Cabal file.

Printed paths are relative to the Cabal file.  Initial motivation was a Git
pre-commit hook which checks that all referenced files are being committed.
