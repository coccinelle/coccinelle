FROM debian:testing
RUN apt-get update && apt-get install --yes autoconf automake unzip aspcud \
  rsync git mercurial darcs build-essential sudo vim curl
RUN adduser --disabled-password --gecos ci --shell /bin/bash ci

