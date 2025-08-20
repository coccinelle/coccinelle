set -e

opam install --yes dune

dune build

./configure

git config user.email stdcompat-ci@nodomain.org
git config user.name stdcompat-ci
git add -f `cat _generated-files`

git commit -m "Generated files as produced by configure"

(while read f; do mv _build/default/$f .; done) < _generated-files

git diff
