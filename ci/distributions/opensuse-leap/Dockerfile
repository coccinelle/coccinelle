FROM opensuse/leap
RUN zypper --non-interactive install autoconf automake unzip gzip \
  rsync git mercurial sudo vim curl tar make patch gcc
RUN useradd --create-home --shell /bin/bash ci
