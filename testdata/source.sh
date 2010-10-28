#! /bin/sh -e

echo "# $2"
echo "(*"
cat "$1"
echo "*)"
echo "# $3"
