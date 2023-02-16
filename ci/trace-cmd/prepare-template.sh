set -ex
export DEBIAN_FRONTEND=noninteractive
sudo tee /etc/apt/apt.conf.d/90forceyes <<EOF
APT::Get::Assume-Yes "true";
EOF
sudo apt-get update
sudo apt-get install autoconf automake unzip aspcud opam rsync git \
    mercurial darcs build-essential sudo vim curl flex python3-dev \
    libpython3-dev pkg-config zlib1g-dev libzstd-dev swig libaudit-dev
(
    git clone https://git.kernel.org/pub/scm/libs/libtrace/libtraceevent.git/
    cd libtraceevent
    make
    sudo make install
)
(
    git clone https://git.kernel.org/pub/scm/libs/libtrace/libtracefs.git/
    cd libtracefs
    make
    sudo make install
)
(
    git clone https://git.kernel.org/pub/scm/utils/trace-cmd/trace-cmd.git/
    cd trace-cmd
    make PYTHON_VERS=python3
    sudo make install PYTHON_VERS=python3
)
opam init --auto-setup
opam switch create 5.0.0
opam install --yes ocamlfind stdcompat pyml menhir parmap
