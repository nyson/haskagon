buildWithHastec:
	haste-cabal --config-file=/home/nyson/.haste-cabal.config install
	cp dist/build/haskagon/haskagon dist/test.html

# testSite:
# 	make -C src
# 	chmod +r test.html
# 	mv test.html dist/

deploy: buildWithHastec
	ssh jont.se 'mkdir -p /var/www/haskagon'
	rsync -pr dist/test.html jont.se:/var/www/haskagon/index.html
