set -ex
export DEBIAN_FRONTEND=noninteractive
sudo tee /etc/apt/apt.conf.d/90forceyes <<EOF
APT::Get::Assume-Yes "true";
EOF
sudo apt-get update
sudo apt-get install autoconf automake unzip aspcud opam trace-cmd rsync git \
  mercurial darcs build-essential sudo vim curl libpython3-dev
opam init --auto-setup
opam switch create 5.0.0
opam install --yes ocamlfind stdcompat pyml menhir parmap
