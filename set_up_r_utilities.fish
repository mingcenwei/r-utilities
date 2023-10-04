#!/usr/bin/env fish

set --global dir_project (realpath -- (status dirname))

function set_up_r_utilities --description "Set up R utilities for the directories" --inherit-variable 'dir_project'
	for dir in $argv
		cp --interactive -- {$dir_project,$dir}'/.gitignore'
		mkdir --parents -- "$dir"/{src/{utilities,data-{analysis,preprocessing}},data,export}
		cp --interactive -- {$dir_project,$dir}'/src/data-analysis/template.Rmd'
		cp --interactive -- {$dir_project,$dir}'/src/data-preprocessing/template.Rmd'
		ln --symbolic --interactive --target-directory="$dir/src/utilities/" -- "$dir_project/src/utilities"/*
		for file in "$dir/src/utilities"/*.mutable.R
			set --local file_dereferenced (realpath -- "$file")
			rm -- "$file"
			cp -- "$file_dereferenced" "$file"
		end
	end
end

if test 0 -ne (count $argv)
	set_up_r_utilities $argv
else
	set_up_r_utilities (pwd)
end
