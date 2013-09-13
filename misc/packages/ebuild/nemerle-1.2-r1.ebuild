# Copyright 1999-2013 Gentoo Foundation
# Distributed under the terms of the GNU General Public License v2
# $Header: $

EAPI="5"

USE_DOTNET="net35 net40 net45"

inherit dotnet multilib eutils

DESCRIPTION="A hybrid programming language for .NET / Mono platforms"
HOMEPAGE="http://www.nemerle.org/"

SRC_URI="https://github.com/rsdn/nemerle/archive/v${PV}.tar.gz"

LICENSE="BSD"
SLOT="0"
KEYWORDS="~amd64 ~x86"
IUSE="+binary"

MAKEOPTS="${MAKEOPTS} -j1" #nowarn
DEPEND=">dev-lang/mono-2.11.3"
RDEPEND="${DEPEND}"

pkg_pretend() {
	if [[ ${MERGE_TYPE} != buildonly ]] && has collision-protect ${FEATURES}; then
		if [ -f /usr/bin/ncc]; then
			eerror "FEATURES=\"collision-protect\" is enabled, which will prevent overwriting"
			eerror "symlinks that were formerly managed by eselect opengl. You must disable"
			eerror "collision-protect or remove /usr/bin/ncc"
			die "collision-protect cannot overwrite libGLU$(get_libname)*"
		fi
	fi
}

src_configure() { :; }
src_compile() {
	if ! use binary; then
		elog "Nemerle sources compiling : "
		exbuild NemerleAll-Mono.nproj /t:Stage1
	fi # ! use binary
}

src_install()
{
	elog "Installing libraries"
	if ! use binary; then
		insinto "/usr/$(get_libdir)/mono/${PN}/${FRAMEWORK}"
		doins bin/Release/mono-"${FRAMEWORK}"/Stage1/*.dll || die "installing libraries failed"
		elog "Registering libraries to egac"
		local nemerledll=bin/Release/mono-"${FRAMEWORK}"/Stage1/Nemerle.dll
		egacinstall "${nemerledll}" \
			|| die "couldn't install ${nemerledll} in the global assembly cache"
		local nemerlecompilerdll=bin/Release/mono-"${FRAMEWORK}"/Stage1/Nemerle.Compiler.dll
		egacinstall "${nemerlecompilerdll}" \
			|| die "couldn't install ${nemerlecompilerdll} in the global assembly cache"
		local nemerlemacrosdll=bin/Release/mono-"${FRAMEWORK}"/Stage1/Nemerle.Macros.dll
		egacinstall "${nemerlemacrosdll}" \
			|| die "couldn't install ${nemerlemacrosdll} in the global assembly cache"
		elog "Installing ncc"
		dodoc README AUTHORS INSTALL NEWS
		into /usr
		doins bin/Release/mono-"${FRAMEWORK}"/Stage1/ncc.exe
	else # binary
		case ${FRAMEWORK} in
			"3.5") Bootstrap="boot";;
			"4.0") Bootstrap="boot-4.0";;
			"4.5")
				ewarn "there is no 4.5 binaries, using 4.0"
				FRAMEWORK="4.0"
				Bootstrap="boot-4.0"
				;;
		esac
		insinto "/usr/$(get_libdir)/mono/${PN}/${FRAMEWORK}"
		doins ${Bootstrap}/*.dll || die "installing libraries failed"
		elog "Registering libraries to egac"
		local nemerledll=${Bootstrap}/Nemerle.dll
		egacinstall "${nemerledll}" \
			|| die "couldn't install ${nemerledll} in the global assembly cache"
		local nemerlecompilerdll=${Bootstrap}/Nemerle.Compiler.dll
		egacinstall "${nemerlecompilerdll}" \
			|| die "couldn't install ${nemerlecompilerdll} in the global assembly cache"
		local nemerlemacrosdll=${Bootstrap}/Nemerle.Macros.dll
		egacinstall "${nemerlemacrosdll}" \
			|| die "couldn't install ${nemerlemacrosdll} in the global assembly cache"
		elog "Installing ncc"
		dodoc README AUTHORS INSTALL NEWS
		into /usr
		doins ${Bootstrap}/ncc.exe
	fi
	make_wrapper ncc "mono /usr/$(get_libdir)/mono/${PN}/${FRAMEWORK}/ncc.exe \"\$@\""
}
