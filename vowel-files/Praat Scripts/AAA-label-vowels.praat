form Input 
	boolean Modify_existing_TextGrids 1
	sentence DirName G:\My Drive\Research\Projects\LDC Article\Proto PR Vowel recordings\needs logged\
endform

# RAJKA -- because the variable dirName$ contains spaces, you must put double quotes around it every
#	time you use it as an argument in a command (unless it's the last argument, in which case you
#	don't need the quotes because it assumes the last argument is everything left on the line)
# Immediately below, it IS the last argument to the command <Create Strings as file list...>, so you don't need quotes.

Create Strings as file list... fileList 'dirName$'*.wav
nFiles = Get number of strings
for i to nFiles
	# get sound filename
	select Strings fileList
	fileName$ = Get string... i
	Read from file... 'dirName$''fileName$'
	name$ = selected$("Sound")

	# if TextGrid doesn't exist, create, and
	# if TextGrid does exist, but modify box is checked, modify
	if fileReadable("'dirName$''name$'.TextGrid")
		if (modify_existing_TextGrids)
			Read from file... 'dirName$''name$'.TextGrid
			plus Sound 'name$'
			Edit
			pause Modify TextGrid as desired
			select TextGrid 'name$'
			Write to text file... 'dirName$''name$'.TextGrid
			plus Sound 'name$'
			Remove
		endif
	else
		select Sound 'name$'
		To TextGrid... vowels
		plus Sound 'name$'
		Edit
		pause Label vowels as intervals (make sure label is nonempty)
		select TextGrid 'name$'
		Write to text file... 'dirName$''name$'.TextGrid
		plus Sound 'name$'
		Remove
	endif
endfor

# clean up
select Strings fileList
Remove