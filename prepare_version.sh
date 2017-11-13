#! /usr/bin/env bash
#
# parse_changelog.sh
# Copyright (C) 2017 Maël Valais <mael.valais@gmail.com>
#
# This script will do the necessary to update the version number.
# At first, the version number was only stored using the git tag.
# But because of opam, the version number must be set in _oasis
# before I do `git describe --tags`...
# So I wrote this script to ease the process of 'Bumping' the version
# number.
#

I='\033[94m+\033[0m' # blue
Q='\033[93m?\033[0m' # yellow
E='\033[91m!\033[0m' # red

# Usage: parse_changelog <changelog_file_name> <version>
# The changelog entry that starts with 'version' will be printed to stdout.
function parse_changelog() {
    CHANGELOG=$1
    VERSION=$2
    in_block=false
    while IFS= read line; do
        if echo "$line" | grep "^$VERSION" ; then
            echo
            echo "$line"
            in_block=true
        elif [ $in_block = true ]; then
            if echo "$line" | grep '^v[0-9]\.[0-9]\.[0-9]' >/dev/null; then
                return
            fi
            echo "$line"
        fi
    done < $CHANGELOG
}

# get_latest_version <changelog_file_name>
# will print to stdout the first vX.Y.Z of the <changelog_file_name> file.
function get_latest_version() {
    head -1 $1 | sed 's/^\(v[0-9]\.[0-9]\.[0-9][^ ]*\).*$/\1/'
}

while [ -n "$1" ]; do
    case $1 in
        --help | -h)
            cat <<EOF
Usage: $0 [-h | --help] [vX.Y.Z]

If no vesion number vX.Y.Z is given, the script will pick the first version
number it finds in CHANGELOG.

This script will help you to successively:
1. change the _oasis version number
2. regenerate the related files
3. (optional) commit changes with message 'Bump to vX.Z.Y'
4. (optional) create a tag 'vX.Z.Y' with the content of the last CHANGELOG
   entry
EOF
            exit 0
            ;;
        v*)
            VERSION=$1
            # Check that it has the right format.
            if ! echo "$VERSION" | grep '^v[0-9]\.[0-9]\.[0-9]'>/dev/null; then
                echo -e "${E} Error, '$VERSION' should be of the form 'vX.Y.Z'"
                exit 1
            fi
            ;;
        *)
            echo "Unknown argument: '$1'"
            $0 --help
            exit 1
            ;;
    esac
    shift
done

# First, check that repo is not dirty because we don't want to commit
# changes that are not part of the version change process.
echo -e "${I} Checking if uncommitted changes..."
if ! git diff-index --quiet HEAD --; then
    echo -e "${E} Repo is dirty; please commit or stash changes."
    exit 1
else
    echo -e "${I} Repo is clean."
fi

if [ "x$VERSION" = x ]; then
    echo -e "${I} No version number given as argument. Looking at CHANGELOG first line..."
    if ! VERSION=$(head -1 CHANGELOG | grep '^v[0-9]\.[0-9]\.[0-9]$'); then
        echo -e "${E} No version number found in first line of CHANGELOG."
        echo -e "${E} Please give a version number as argument."
        exit 1
    fi
    echo -e "${Q} Should we use \033[92m${VERSION}\033[0m from CHANGELOG? [Y/n]"
    read answerAutoNumber
    case $answerAutoNumber in n)
        exit 1 ;;
    esac
fi

# At this point, we know what VERSION we are going to use.

# For the opam version, remove the leading 'v'.
OPAM_VERSION=$(echo $VERSION | cut -c 2-)
echo -e "${I} Changing the 'Version:' field in '_oasis' from \033[92m$(oasis query version)\033[0m to \033[92m$OPAM_VERSION\033[0m"
sed "s/^\(Version: *\)[0-9][0-9\.]*$/\1$OPAM_VERSION/" _oasis > a && mv a _oasis
echo -e "\033[90m$(git diff -U0 _oasis | grep "^\(\+\|-\)[^+-]")\033[0m"

echo -e "${I} Adding the date to the version: \033[92m  \033[0m"
sed "s/^\(${VERSION//./\\.}.*$\)/\1 (`date -I`)/" CHANGELOG > a && mv a CHANGELOG
echo -e "\033[90m$(git diff -U0 CHANGELOG | grep "^\(\+\|-\)[^+-]")\033[0m"

echo -e "${I} Running 'oasis setup' to update setup.ml, src/lib/META\033[0m"
oasis setup

echo -e "${I} Running 'oasis2opam --local -y' to update opam/opam\033[0m"
oasis2opam --local -y
echo -e "\033[90m$(git diff -U0 opam/ | grep "^\(\+\|-\)[^+-]")\033[0m"

echo -e "${I} Changes done."
echo -e "${Q} Do you want to commit using message '\033[92mBump to $VERSION\033[0m'? [Y/n]"
read answerCommit
echo $answerCommit
case $answerCommit in
    y | Y)
        echo -e "${I} Running 'git add setup.ml _oasis src/lib/META opam/*'\033[0m"
        git add setup.ml _oasis opam/*
        find . -name META -exec git add {} \;
        echo -e "${I} Commiting with message '\033[92mBump to $VERSION\033[0m'"
        git commit -m "Bump to $VERSION"
        ;;
    *) echo -e "${I} Doing nothing.";;
esac

echo -e "${I} Parsing the changelog for version '\033[92mBump to $VERSION\033[0m'..."
parse_changelog CHANGELOG "$VERSION" > tag_content
echo -e "${Q} Do you want to create a signed tag '\033[92mBump to $VERSION\033[0m' using the following message (u for unsigned tag; y for signed tag): [Y/n/u]"
while IFS= read line; do
    echo -e "\033[90m${line}\033[0m"
done < tag_content

read ans
case "$ans" in
    y|Y|u)
        echo -e "${I} Creating the tag..."
        SIGN=-s
        if [ "$ans" = "u" ]; then SIGN=; echo -e "${I} (unsigned tag)"; fi
        git tag $SIGN "$VERSION" --file tag_content
        ;;
    *) echo -e "${I} No tag created.";;
esac
rm tag_content

echo -e "======== Next steps: =========="
echo -e "${I} (1) push tag:"
echo -e "git push origin $VERSION:$VERSION"
echo -e ""
echo -e "${I} (2) publish (${E} WAIT FOR THE TRAVIS BUILD ${E}):"
echo -e "opam publish prepare"
echo -e "opam publish submit touist.$OPAM_VERSION"
echo -e "brew bump-formula-pr touist/touist/touist --url=https://github.com/touist/touist/archive/${$VERSION}.tar.gz"
echo -e ""
echo -e "${I} (1 bis) cancel the commit/tag made in (1):"
echo -e "git reset @^"
echo -e "git tag -d $VERSION"
echo -e "git push origin :$VERSION"
