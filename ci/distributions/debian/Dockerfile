FROM debian:bookworm
RUN apt-get update && apt-get install --yes autoconf automake unzip aspcud \
  rsync git mercurial darcs build-essential sudo vim curl python3 \
  libpython3-dev hevea texlive-fonts-extra
RUN adduser --disabled-password --gecos ci --shell /bin/bash ci
# Get rid of the "setrlimit(RLIMIT_CORE): Operation not permitted" warning
# See https://bugzilla.redhat.com/show_bug.cgi?id=1773148
RUN echo Set disable_coredump false >/etc/sudo.conf
