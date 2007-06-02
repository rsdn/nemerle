# author: Kamil Dworakowski (@ gmail)
#
# to generate a tags file for nemerle files writen in indentation based syntax
# anyway, it works also with normal syntax
# currently gentags.awk returns only tags for (public|internal) (class|variant|module)
# To make vim search tags in two tags files, add to local _vimrc:
# set tags=tags,tags_i
# Then if a tag is not found in tags file, it will be searched for in tags_i.
find . -name *.n | xargs awk -f gentags.awk | sort > tags_i
