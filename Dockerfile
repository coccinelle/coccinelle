FROM ubuntu:16.04
MAINTAINER Wen Yang <yellowriver2010@hotmail.com>

# Install standard packages.
RUN apt-get update && apt-get install -y python-all git sudo make autoconf

# Install extra deps for imaging
RUN apt-get install -y  ocaml-native-compilers ocaml-findlib menhir \
    libmenhir-ocaml-dev libpcre-ocaml-dev libparmap-ocaml-dev \
    libpython-dev

ENV COCCI_ROOT=/coccinelle
RUN mkdir -p ${COCCI_ROOT}
RUN git clone https://github.com/coccinelle/coccinelle.git ${COCCI_ROOT}
RUN cd ${COCCI_ROOT} && ./autogen && ./configure && make && make install

CMD bash
