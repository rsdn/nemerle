# author: Kamil Dworakowski (@gmail)
#
# generates a half finished tags file in a basic format of ctags 
# (not an exuberant ctags format)
# the output needs to be sorted to be valid tags file
BEGIN { 
    print ("!_TAG_FILE_FORMAT\t1\t") 
    print ("!_TAG_FILE_SORTED\t1\t/0 - unsorted, 1 - sorted/") 
}
# \w 	alphanumeric character or '_'
# [:space:] 	Space characters (such as space, TAB, ... ). 
/.*public[[:space:]]+class[[:space:]]+\w+.*/ {
    match($0, /.*(public[[:space:]]+class[[:space:]]+)(\w+).*/, arr )
# Format: {tagname}	{TAB} {tagfile} {TAB} {tagaddress}
    print( arr[2] "\t" FILENAME "\t" "/" arr[1] arr[2] "/" )
}
