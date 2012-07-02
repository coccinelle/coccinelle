#! /bin/sh

# fallback date (in RFC format)
VERSION=`date "+%a, %d %b %Y %H:%M:%S %z"`

# use the date information from git, if git is present
if test -d ".git" && which git &>/dev/null; then
VERSION=`git log -1 --date-order --date=rfc --pretty="format:%cd"`
fi

echo "let version_date = \"$VERSION\""
