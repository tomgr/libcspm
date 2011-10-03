#!/bin/bash -e

# First, make sure the version has been changed in the .cabal files
# Also, make sure to tag it using git (and then update the .cabal files).

VERSION=0.1.1

GITTAG="release-$VERSION"

echo "Using version $VERSION and tag $GITTAG"
echo "Have you updated the version number and the changelog? Type 'yes' if you have!"
read version_response

if [ "$version_response" != "yes" ]; then
    echo "Go and update the version number"
    exit 1
fi

require_clean_work_tree () {
    git rev-parse --verify HEAD >/dev/null || exit 1
    git update-index -q --ignore-submodules --refresh
    err=0

    if ! git diff-files --quiet --ignore-submodules
    then
        echo >&2 "Cannot $1: You have unstaged changes."
        err=1
    fi

    if ! git diff-index --cached --quiet --ignore-submodules HEAD --
    then
        if [ $err = 0 ]
        then
            echo >&2 "Cannot $1: Your index contains uncommitted changes."
        else
            echo >&2 "Additionally, your index contains uncommitted changes."
        fi
        err=1
    fi

    if [ $err = 1 ]
    then
        test -n "$2" && echo >&2 "$2"
        exit 1
    fi
}

require_clean_work_tree

sed -i".old" -e "s/Version:.*/Version: $VERSION/g" libcspm.cabal 
sed -i".old" -e "s/tag:.*/tag: $GITTAG/g" libcspm.cabal
rm libcspm.cabal.old

sed -i".old" -e "s/Version:.*/Version: $VERSION/g" cspmchecker/cspmchecker.cabal 
sed -i".old" -e "s/tag:.*/tag: $GITTAG/g" cspmchecker/cspmchecker.cabal
rm cspmchecker/cspmchecker.cabal.old

cabal clean
cabal configure
cabal build
cabal sdist
cabal upload --check dist/libcspm-$VERSION.tar.gz

pushd cspmchecker >/dev/null
    cabal clean
    cabal configure
    cabal build
    cabal sdist
    cabal upload --check dist/cspmchecker-$VERSION.tar.gz
popd >/dev/null

git stage cspmchecker/cspmchecker.cabal
git stage libcspm.cabal
git commit -m "Updating version number for release"
git tag add $GITTAG

cabal upload dist/libcspm-$VERSION.tar.gz
pushd cspmchecker
    cabal upload --check dist/cspmchecker-$VERSION.tar.gz
popd

git push
git push origin $GITTAG
