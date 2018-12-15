FROM debian:stretch

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update && \
    apt-get install -y locales && \
    apt-get clean && rm -fr /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN locale-gen ja_JP.UTF-8
ENV LANG ja_JP.UTF-8
ENV LC_CTYPE ja_JP.UTF-8
RUN localedef -f UTF-8 -i ja_JP ja_JP.utf8

RUN apt-get update && \
    apt-get install -y curl git ca-certificates openssl && \
    apt-get clean && rm -fr /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN apt-get update && \
    curl -sSL https://get.haskellstack.org/ | sh && \
    apt-get clean && rm -fr /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN mkdir -p /opt/build
WORKDIR /opt/build

ADD package.yaml /opt/build
ADD stack.yaml /opt/build

ENV HOME /opt/build
RUN stack setup
RUN stack build --only-dependencies
RUN chmod 777 -R /opt/build
