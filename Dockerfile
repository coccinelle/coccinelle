FROM ocaml/opam2:4.07
LABEL maintainer="Thierry.Martinez@inria.fr"
WORKDIR /home/opam
COPY . /coccinelle/
RUN opam pin add -n file:///coccinelle
RUN opam depext -i coccinelle