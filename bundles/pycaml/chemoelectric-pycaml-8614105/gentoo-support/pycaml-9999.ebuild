# Copyright 1999-2008 Gentoo Foundation
# Distributed under the terms of the GNU General Public License v2
# $Header$

EAPI="2"
SUPPORT_PYTHON_ABIS="1"

inherit findlib python eutils git

IUSE=""

DESCRIPTION="Bindings for Python and OCaml"
HOMEPAGE="http://github.com/chemoelectric/pycaml"
SRC_URI=""
EGIT_REPO_URI="git://github.com/chemoelectric/pycaml.git"

LICENSE="LGPL-2.1"
SLOT="0"
KEYWORDS="~amd64 ppc x86"

RDEPEND=">=dev-ml/ocaml-make-6.29.3"
DEPEND=">=dev-lang/ocaml-3.11.1
        >=dev-lang/python-2.6.4
        ${RDEPEND}"

S="${WORKDIR}/${PN}"


pkg_setup() {
	if ! built_with_use --missing true dev-lang/ocaml ocamlopt; then
		eerror "${PN} needs to be built with native code support from ocaml"
		eerror "You first need to have a native code ocaml compiler."
		eerror "You need to install dev-lang/ocaml with ocamlopt useflag on."
		die "Please install ocaml with ocamlopt useflag"
	fi
}

src_compile() {
	compile_abi_pycaml() {
		abi=`echo "${PYTHON_ABI}" | sed -e 's/\./_/g'`
		name="${PN}${abi}"
		cd "${WORKDIR}"
		cp -r "${PN}" "${name}"
		cd "${name}"
		emake -j1 PYVER="${PYTHON_ABI}" PYVER_PACK="${abi}" \
		      get_libdir="$(get_libdir)" || die "emake failed"
	}
	python_execute_function compile_abi_pycaml

	compile_pycaml() {
		cd "${S}"
		emake -j1 PYVER="${PYVER}" PYVER_PACK="" \
		      get_libdir="$(get_libdir)" || die "emake failed"
	}
	python_version
	compile_pycaml
}

src_install() {
	python_need_rebuild

	# Use findlib to install properly, especially to avoid
	# the shared library mess
	findlib_src_preinst

	install_abi_pycaml() {
		abi=`echo "${PYTHON_ABI}" | sed -e 's/\./_/g'`
		name="${PN}${abi}"
		cd "${WORKDIR}/${name}"
		ocamlfind install "${name}" \
			  dllpycaml"${abi}"_stubs.so* libpycaml"${abi}"_stubs.a \
			  pycaml"${abi}".a  pycaml"${abi}".cma pycaml"${abi}".cmxa \
			  pycaml.cmi pycaml.cmo pycaml.cmx pycaml.ml pycaml.mli \
			  pycaml.o pycaml_stubs.c pycaml_stubs.h pycaml_stubs.o META
	}
	python_execute_function install_abi_pycaml

	install_pycaml() {
		cd "${S}"
		ocamlfind install "${PN}" \
			  dllpycaml_stubs.so* libpycaml_stubs.a pycaml.a pycaml.cma \
			  pycaml.cmi pycaml.cmo pycaml.cmx pycaml.cmxa pycaml.ml pycaml.mli \
			  pycaml.o pycaml_stubs.c pycaml_stubs.h pycaml_stubs.o META
	}
	python_version
	install_pycaml
}
