#!/usr/bin/env bash
set -e

VERSION=$1

if [ "a$VERSION" = "a" -o "a$VERSION" = "a-h" -o "a$VERSION" = "a--help" ]; then
    echo "Usage: $0 <version number>"
    echo "Example: $0 `cat version`"
    exit 1
fi

PACKAGE=coccinelle-$VERSION

WORKING_DIRECTORY=/tmp/$PACKAGE

WEBBASE=~/website
WEBSITE=$WEBBASE/distrib

echo The release script will write $VERSION to the file ./version and will create
echo a tag $VERSION if there is no such tag already.
echo If you want to delete a previously tagged release, run the following command:
echo "  git tag -d $VERSION"
echo
echo The release script will work in the directory $WORKING_DIRECTORY and
echo will work only if this directory does _not_ exist already. If you
echo want to delete a previously created directory, run the following command:
echo "  rm -rf $WORKING_DIRECTORY"
echo
echo The release script will _not_ push the updated repository.
echo You may want to run the following command after the release:
echo "  git push --tags"
echo
if [ -d "$WEBSITE" ]; then
  echo For the website, you need to manually update those files:
  echo "  $WEBBASE/download.php"
  echo "  $WEBSITE/change.html"
  echo Then commit the changes.
  echo
fi

set -x

mkdir $WORKING_DIRECTORY

echo $VERSION > version
./autogen --ignore_localversion

if ! git rev-parse $VERSION &>/dev/null; then
    git add version
    #git add setup/Makefile.in
    git commit -m "Release $VERSION" || true
    git tag -a -m "Release $VERSION" $VERSION
fi

./configure >/dev/null
make docs >/dev/null
mkdir $WORKING_DIRECTORY/$PACKAGE
cp -r * $WORKING_DIRECTORY/$PACKAGE
rm -f $WORKING_DIRECTORY/$PACKAGE/*.tar.gz
make spatch.opt tools/spgen/source/spgen.opt >/dev/null
cp spatch.opt $WORKING_DIRECTORY/$PACKAGE
cp tools/spgen/source/spgen.opt $WORKING_DIRECTORY/$PACKAGE/tools/spgen/source
cp ocaml/*.cmi ocaml/*.cmx $WORKING_DIRECTORY/$PACKAGE/ocaml
tar -czf $PACKAGE-bin-x86.tar.gz -C $WORKING_DIRECTORY $PACKAGE
rm -rf $WORKING_DIRECTORY/$PACKAGE
make clean >/dev/null
mkdir $WORKING_DIRECTORY/$PACKAGE
cp -r * $WORKING_DIRECTORY/$PACKAGE
rm -f $WORKING_DIRECTORY/$PACKAGE/*.tar.gz
make spatch tools/spgen/source/spgen >/dev/null
cp spatch $WORKING_DIRECTORY/$PACKAGE
cp tools/spgen/source/spgen $WORKING_DIRECTORY/$PACKAGE/tools/spgen/source
cp ocaml/*.cmi ocaml/*.cmo $WORKING_DIRECTORY/$PACKAGE/ocaml
tar -czf $PACKAGE-bin-bytecode-`ocamlc -vnum`.tar.gz -C $WORKING_DIRECTORY $PACKAGE
rm -rf $WORKING_DIRECTORY/$PACKAGE
make clean >/dev/null
mkdir $WORKING_DIRECTORY/$PACKAGE
cp -r * $WORKING_DIRECTORY/$PACKAGE
rm -f $WORKING_DIRECTORY/$PACKAGE/*.tar.gz
tar -czf $PACKAGE.tar.gz -C $WORKING_DIRECTORY $PACKAGE
rm -rf $WORKING_DIRECTORY/$PACKAGE
if [ -d "$WEBSITE" ]; then
   cp $PACKAGE.tar.gz $WEBSITE
   cp $PACKAGE-bin-x86.tar.gz $WEBSITE
   ( cd $WEBSITE; git add $PACKAGE.tar.gz $PACKAGE-bin-x86.tar.gz )
fi
