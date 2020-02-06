ARG from
FROM $from
RUN echo ci      ALL=\(ALL\) NOPASSWD:ALL >/etc/sudoers
USER ci
RUN curl >~/opam --location \
  https://github.com/ocaml/opam/releases/download/2.0.6/opam-2.0.6-x86_64-linux
RUN chmod +x ~/opam
RUN sudo mv ~/opam /usr/local/bin/opam
RUN opam init --disable-sandboxing --auto-setup --dot-profile=/home/ci/.bash_env
SHELL ["/bin/bash", "-c"]
ENV BASH_ENV /home/ci/.bash_env
ARG ocaml_version
RUN opam switch create $ocaml_version
# TODO Sadly, ocamlfind looks to be a dependency for Coccinelle
# (needed for parmap bundle)
RUN opam install ocamlfind
ARG stdcompat
RUN [ x$stdcompat != xyes ] || opam install --yes stdcompat
ARG pyml
RUN [ x$pyml != xyes ] || opam install --yes pyml
ARG menhir
RUN [ x$menhir != xyes ] || opam install --yes menhir
ARG parmap
RUN [ x$parmap != xyes ] || opam install --yes parmap
