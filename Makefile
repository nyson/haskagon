testSite:
	make -C src
	chmod +r test.html
	mv test.html dist/

deploy: testSite
	ssh jont.se 'mkdir -p /var/www/haskagon'
	rsync -pr dist/test.html jont.se:/var/www/haskagon/index.html
