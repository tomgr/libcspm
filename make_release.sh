#!/bin/bash -e

VERSION=1.0.0

GITTAG="release-$VERSION"

echo "Using version $VERSION and tag $GITTAG"
echo "Have you updated the version number and the changelog? Type 'yes' if you have!"
read version_response

if [ "$version_response" != "yes" ]; then
    echo "Go and update the version number"
    exit 1
fi

git status

echo "Did git status contain pending changes? Nb. this script will commit make_release.sh (with the new version number)."
read version_response

if [ "$version_response" != "yes" ]; then
    echo "Commit them"
    exit 1
fi

sed -i".old" -e "s/Version:.*/Version: $VERSION/g" libcspm.cabal 
sed -i".old" -e "s/tag:.*/tag: $GITTAG/g" libcspm.cabal
rm libcspm.cabal.old

sed -i".old" -e "s/Version:.*/Version: $VERSION/g" cspmchecker/cspmchecker.cabal 
sed -i".old" -e "s/tag:.*/tag: $GITTAG/g" cspmchecker/cspmchecker.cabal
sed -i".old" -e "s/libcspm >= .*,/libcspm >= $VERSION,/g" cspmchecker/cspmchecker.cabal 
rm cspmchecker/cspmchecker.cabal.old

rm -rf dist
cabal sdist
cabal upload --check dist/libcspm-$VERSION.tar.gz

pushd cspmchecker >/dev/null
    rm -rf dist
    cabal sdist
    cabal upload --check dist/cspmchecker-$VERSION.tar.gz
popd >/dev/null

git stage make_release.sh
git stage cspmchecker/cspmchecker.cabal
git stage libcspm.cabal
git commit -m "Updating version number for release"
git tag $GITTAG

cabal upload dist/libcspm-$VERSION.tar.gz
pushd cspmchecker
    cabal upload dist/cspmchecker-$VERSION.tar.gz
popd

git push
git push origin $GITTAG
