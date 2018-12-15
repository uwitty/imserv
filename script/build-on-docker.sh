#!/bin/sh
docker run --rm \
	   -v `pwd`:`pwd` \
	   --workdir `pwd` \
	   --user `id -u`:`id -g` \
	   test/imserv-build stack build --allow-different-user
	   #test/imserv-build stack clean --allow-different-user


