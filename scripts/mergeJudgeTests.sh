#!/bin/bash

dir="$(dirname $0)/.."
cd "$dir"

#opens local file as tests, so that you can use sp to put a single line in if you are me and have my vimrc which you are not
for new in src/*.tested; do
	oldfile="src/"$(echo $(basename "$new") | sed "s/\.tested$//")
	if ! [[ -z $(diff "$new" "$oldfile") ]]; then
		echo "diffing $new"
		vim -d "$new"  "$oldfile"
	else
		echo "skipping $new"
	fi

done

