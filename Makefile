
all:
	echo "no all..."
	false

send: dist send-www send-dist

send-www:
	$(MAKE) -C doc
	rm -rf www
	mkdir -p www/{styles,course}
	cp doc/styles/*.{css,png} www/styles/
	cp doc/*.{html,ps,pdf} www/
	cp doc/course/*.html www/course/
	for f in www/*.pdf www/*.ps ; do gzip <$$f > $$f.gz ; done
	(cd www; tar zcf - .) | ssh lilith \
		'cd /home/services/httpd/html; cat > download/nemerle-web.tar.gz; tar xzf download/nemerle-web.tar.gz'
	rm -rf www

name = nemerle
svn2log = python aux/svn2log.py changelog.xml -u aux/users

changelog:
	svn up
	svn log -v --xml > changelog.xml
	$(svn2log) -p /nemerle/trunk -x npc -x doc
	$(svn2log) -p /nemerle/trunk/npc -o npc/ChangeLog -x ncc
	$(svn2log) -p /nemerle/trunk/ncc -o ncc/ChangeLog -r npc/ncc=ncc
	$(svn2log) -p /nemerle/trunk/doc -o doc/ChangeLog
	rm -f changelog.xml

dist: changelog
	if svn status 2>&1 | grep -qv '^?' ; then \
	  echo "Some files modified"; \
	  false; \
	else \
	  ver=`svn info . | awk '/^Revision:/ { print $$2 }'`; \
	  rm -rf $(name)-$$ver; \
	  svn export . $(name)-$$ver; \
	  for f in . npc ncc doc ; do \
	    cp $$f/ChangeLog $(name)-$$ver/$$f; \
	  done; \
	  tar zcf $(name)-$$ver.tar.gz $(name)-$$ver; \
	  rm -rf $(name)-$$ver; \
	  ls -l $(name)-$$ver.tar.gz; \
	fi

send-dist:
	if [ "X`find -maxdepth 1 -mindepth 1 -name 'nemerle-[0-9]*.tar.gz'`" = X ] ; then \
		echo "No files available" ; \
		false ; fi
	scp `find -maxdepth 1 -mindepth 1 -name 'nemerle-[0-9]*.tar.gz'` \
		lilith:/home/services/httpd/html/download/
	scp `find -maxdepth 1 -mindepth 1 -name 'nemerle-[0-9]*.tar.gz' | sort | tail -1` \
		lilith:/home/services/httpd/html/download/nemerle-latest.tar.gz
	mv `find -maxdepth 1 -mindepth 1 -name 'nemerle-[0-9]*.tar.gz'` ~/backup/nemerle/

clean:
	$(MAKE) -C doc clean
	$(MAKE) -C npc clean
	$(MAKE) -C ncc clean
