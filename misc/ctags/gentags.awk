# author: Kamil Dworkakowski (@gmail)
#
# generates an tags file in a basic format of ctags that needs to be sorted
# (not an exuberant ctags format)
# the output needs to be sorted to be valid tags file
BEGIN { 
    print ("!_TAG_FILE_FORMAT\t1\t") 
    print ("!_TAG_FILE_SORTED\t1\t/0 - unsorted, 1 - sorted/") 
}
# \w 	alphanumeric character or '_'
# [:space:] 	whitespace characters
/.*(public|internal)[[:space:]]+(class|variant|module)[[:space:]]+\w+.*/ {
    match($0, /.*((class|variant|module)[[:space:]]+)(\w+).*/, arr )
# Format is: {tagname}	{TAB} {tagfile} {TAB} {tagaddress}
    print( arr[3] "\t" FILENAME "\t" "/" arr[1] arr[3] "/" )
}
