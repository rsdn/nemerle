
all:
	echo "no all..."
	false

send: dist send-www send-dist

send-www:
	$(MAKE) -C doc
	rm -rf www
	mkdir -p www/styles
	cp doc/styles/*.{css,png} www/styles/
	cp doc/*.{html,ps,pdf} www/
	for f in www/*.pdf www/*.ps ; do gzip <$$f > $$f.gz ; done
	(cd www; tar zcf - .) | ssh ep \
		'cd www/nemerle; cat > download/nemerle-web.tar.gz; tar xzf download/nemerle-web.tar.gz'
	(cd www; tar zcf - .) | ssh lilith \
		'cd /home/services/httpd/html; cat > download/nemerle-web.tar.gz; tar xzf download/nemerle-web.tar.gz'
	rm -rf www

name = nemerle

dist:
	if svn status 2>&1 | grep -qv '^?' ; then \
	  echo "Some files modified"; \
	  false; \
	else \
	  svn up; \
	  ver=`svn info . | awk '/^Revision:/ { print $$2 }'`; \
	  rm -rf $(name)-$$ver; \
	  svn export . $(name)-$$ver; \
	  tar zcf $(name)-$$ver.tar.gz $(name)-$$ver; \
	  rm -rf $(name)-$$ver; \
	  ls -l $(name)-$$ver.tar.gz; \
	fi

send-dist:
	if [ "X`find -maxdepth 1 -mindepth 1 -name 'nemerle-[0-9]*.tar.gz'`" = X ] ; then \
		echo "No files available" ; \
		false ; fi
	scp `find -maxdepth 1 -mindepth 1 -name 'nemerle-[0-9]*.tar.gz'` ep:www/nemerle/download/
	scp `find -maxdepth 1 -mindepth 1 -name 'nemerle-[0-9]*.tar.gz' | sort | tail -1` \
		ep:www/nemerle/download/nemerle-latest.tar.gz
	scp `find -maxdepth 1 -mindepth 1 -name 'nemerle-[0-9]*.tar.gz'` \
		lilith:/home/services/httpd/html/download/
	scp `find -maxdepth 1 -mindepth 1 -name 'nemerle-[0-9]*.tar.gz' | sort | tail -1` \
		lilith:/home/services/httpd/html/download/nemerle-latest.tar.gz
	mv `find -maxdepth 1 -mindepth 1 -name 'nemerle-[0-9]*.tar.gz'` ~/backup/nemerle/

clean:
	$(MAKE) -C doc clean
	$(MAKE) -C npc clean
