#! /usr/bin/env bash
#
# parse_changelog.sh
# Copyright (C) 2017 Maël Valais <mael.valais@gmail.com>
#
# This script will do the necessary to update the version number.
# At first, the version number was only stored using the git tag.
# But because of opam, the version number must be set in touist.opam.
# before I do `git describe --tags`...
# So I wrote this script to ease the process of 'Bumping' the version
# number.
#
gray='\033[90m'
red='\033[91m'
green='\033[92m'
yel='\033[93m'
blu='\033[94m'
purp='\033[95m'
end='\033[0m'
I="$blu+$end" # blue
Q="$yel?$end" # yellow
E="$red!$end" # red

# Usage: parse_changelog <changelog_file_name> <version>
# The changelog entry that starts with 'version' will be printed to stdout.
function parse_changelog() {
    CHANGELOG=$1
    VERSION=$2
    in_block=false
    while IFS= read -r line; do
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
    done < "$CHANGELOG"
}

while [ -n "$1" ]; do
    case $1 in
        --help | -h)
            cat <<EOF
Usage: $0 [-h | --help] [vX.Y.Z][+betaX]

If no vesion number vX.Y.Z is given, the script will pick the first version
number it finds in CHANGELOG.

This script will help you to successively:
1. change the touist.opam version number
2. add the date in CHANGELOG
3. (optional) commit changes with message 'Bump to vX.Z.Y'
4. (optional) create a tag 'vX.Z.Y' with the content of the last CHANGELOG
   entry

Using '+betaX', notes:
- the changelog date won't be added as it is not a 'final' release
EOF
            exit 0
            ;;
        v*)
            VERSION=$1
            # Check that it has the right format.
            if ! echo "$VERSION" |  grep '^v[0-9]\.[0-9]\.[0-9]\(\+beta[0-9]*\)*$'>/dev/null; then
                echo -e "${E} Error, '$VERSION' should be of the form 'vX.Y.Z[+betaX]'"
                exit 1
            else
                SUFFIX=${VERSION/v[0-9].[0-9].[0-9]/}
                VERSION=${VERSION/$SUFFIX/}
                echo -e "${I} Version:${yel}${VERSION}${end}${green}${SUFFIX}${end}"
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
        echo -e "${E} No version number of form 'vX.Y.Z' found in first line of CHANGELOG."
        echo -e "${E} Please give a version number as argument."
        exit 1
    fi
    echo -e "${Q} Should we use ${green}${VERSION}${end} from CHANGELOG? [Y/n]"
    read -r answerAutoNumber
    case $answerAutoNumber in n)
        exit 1 ;;
    esac
fi

# At this point, we know what VERSION we are going to use.
echo -e "${I} Parsing the changelog for version '${green}${VERSION}${end}'..."
parse_changelog CHANGELOG "$VERSION" > tag_content
# In case the date has already been added to CHANGELOG, remove it for the tag
sed 's/\(v[0-9]\.[0-9]\.[0-9][^ ]*\).*/\1/' tag_content > temp && mv temp tag_content

# For the opam version, remove the leading 'v'.
OPAM_VERSION=$(echo "$VERSION$SUFFIX" | cut -c 2-)
PREV_OPAM_VERSION=$(grep "^version:" touist.opam | sed 's/^version: *\"\(.*\)\"$/\1/')
echo -e "${I} Changing the 'version:' field in 'touist.opam' from ${purp}${PREV_OPAM_VERSION}${end} to ${green}${OPAM_VERSION}${end}"
sed "s/^\\(version: *\\)\"[0-9][0-9\\.]*[^ ]*\"$/\\1\"${OPAM_VERSION}\"/" touist.opam > a && mv a touist.opam
echo -e "${gray}$(git diff -U0 touist.opam | grep '^\(\+\|-\)[^+-]')${end}"

if [ -z "$SUFFIX" ]; then # If 'beta', then no date change
    echo -e "${I} Change version line in CHANGELOG from ${purp}${OPAM_VERSION}${end} to ${green}${OPAM_VERSION} ($(date -I))${end}:"
    sed "s/^\\(${VERSION//./\\.}[^ ]*\\).*$/\\1 ($(date -I))/" CHANGELOG > a && mv a CHANGELOG
    echo -e "${gray}$(git diff -U0 CHANGELOG | grep '^\(\+\|-\)[^+-]')${end}"
else
    echo -e "${I} Warning: you gave a suffix ${red}${SUFFIX}${end} so we don't add the date to CHANGELOG"
    echo -e "${I} Instead, use the suffix to the tag"
    sed "s/^\\(${VERSION//./\\.}[^ ]*\\).*$/\\1${SUFFIX}/g" tag_content > a && mv a tag_content
fi
echo -e "${I} Changes done."
echo -e "${Q} Do you want to commit using message '${green}Bump to $VERSION$SUFFIX${end}'? [Y/n]"
read -r answerCommit
echo "$answerCommit"
case "$answerCommit" in
    y | Y)
        echo -e "${I} Running 'git add *.opam *.descr CHANGELOG'${end}"
        git add ./*.opam ./*.descr CHANGELOG
        echo -e "${I} Commiting with message '${green}Bump to $VERSION$SUFFIX${end}'"
        git commit -m "Bump to $VERSION$SUFFIX"
        ;;
    *) echo -e "${I} Doing nothing.";;
esac

echo -e "${Q} Do you want to create a signed tag '${green}Bump to $VERSION$SUFFIX${end}' using the following message:"
while IFS= read -r line; do
    echo -e "${gray}${line}${end}"
done < tag_content
echo -en "${Q} (u for unsigned tag; y for signed tag): [Y/n/u] "
read -r ans
case "$ans" in
    y|Y|u)
        echo -e "${I} Creating the tag..."
        SIGN=-s
        if [ "$ans" = "u" ]; then SIGN=; echo -e "${I} (unsigned tag)"; fi
        git tag $SIGN "$VERSION$SUFFIX" --file tag_content
        ;;
    *) echo -e "${I} No tag created.";;
esac
rm tag_content

echo -e "======== Next steps: =========="
echo -e "${I} (1) push tag:"
echo -e "git push origin $VERSION$SUFFIX:$VERSION$SUFFIX"
echo -e ""
echo -e "${I} (2) publish (${E} WAIT FOR THE TRAVIS BUILD ${E}):"
echo -e "opam publish prepare"
echo -e "opam publish submit touist.$OPAM_VERSION"
echo -e "brew bump-formula-pr --url=https://github.com/touist/touist/archive/${VERSION}${SUFFIX}.tar.gz --tap=touist/touist touist"
echo -e ""
echo -e "${I} (1 bis) cancel the commit/tag made in (1):"
echo -e "git reset @^"
echo -e "git tag -d $VERSION$SUFFIX"
echo -e "git push origin :$VERSION$SUFFIX"
