#!/bin/sh
set -x -e
version=0.1.0
image_name=test/imserv-build

cp -f ../../package.yaml ./
cp -f ../../stack.yaml ./
docker build -t $image_name .
docker tag $image_name:latest $image_name:$version
