# $Revision: 1.5 $, $Date: 2004/02/04 14:22:32 $
Summary:	Nemerle compiler
Summary(pl):	Kompilator jêzyka Nemerle
Name:		nemerle
Version:	0.0.1592
Release:	1
Epoch:		0
License:	BSD
Group:		Development/Languages
Vendor:		Nemerle Development Team <feedback@nemerle.org>
Source0:	http://nemerle.org/download/%{name}-%{version}.tar.gz
# Source0-md5:	f43617f953680ab1a1414eca5955fab9
BuildArch:	noarch
URL:		http://nemerle.org/
Requires(post):	mono >= 0.29
Requires:	mono >= 0.29
Requires:	%{name}-libs = %{epoch}:%{version}-%{release}
BuildRoot:	%{tmpdir}/%{name}-%{version}-root-%(id -u -n)

%description
Nemerle is a new functional language designed from the ground up for
the .NET platform. Nemerle supports: object oriented and imperative
.NET concepts, variant datatypes, matching, higher order functions and
powerful macro system. It has simple, C-like syntax and makes access
to imperative features easy, and thus is easy to learn.

%description -l pl
Nemerle jest nowym jêzykiem funkcjonalnym zaprojektowym od pocz±tku z
my¶l± o platformie .NET. Nemerle wspiera programowanie obiektowe i
imperatywne, typy wariantowe, dopasowanie wzorca, funkcje wy¿szych
rzêdów oraz pote¿ny system makr. Sk³adnia Nemerle jest prosta,
przypomina trochê C. Nemerle umo¿liwa ³atwy dostêp do swych
imperatywnych oraz obiektowych cech, co powinno u³atwiæ uczenie siê
go.

%package libs
Summary:	Nemerle runtime environment
Summary(pl):	¦rodowisko uruchomieniowe jêzyka Nemerle
BuildArch:	noarch
Group:		Libraries
Requires(post):	mono >= 0.29
Requires:	mono >= 0.29

%description libs
Libraries needed to run programs written in Nemerle.

%description libs -l pl
Biblioteki niezbêdne do uruchamiania programów napisanych w Nemerle.

%prep
%setup -q

%build
./configure \
	--ignore-errors \
	--prefix=%{_prefix} \
	--bindir=%{_bindir} \
	--libdir=%{_libdir} \
	--mandir=%{_mandir}/man1

%install
rm -rf $RPM_BUILD_ROOT

%{__make} install \
	DESTDIR=$RPM_BUILD_ROOT

mv $RPM_BUILD_ROOT%{_bindir}/ncc{,.exe}

cat > $RPM_BUILD_ROOT%{_bindir}/ncc <<EOF
#!/bin/sh
exec mono %{_bindir}/ncc.exe "\$@"
EOF

for f in $RPM_BUILD_ROOT%{_bindir}/*.exe $RPM_BUILD_ROOT%{_libdir}/*.dll ; do
	touch $f.so
done

%clean
rm -rf $RPM_BUILD_ROOT

%post
mono --aot %{_libdir}/Nemerle.Compiler.dll || :
mono --aot %{_libdir}/Nemerle.Macros.dll || :
mono --aot %{_bindir}/ncc.exe || :

%post libs
mono --aot %{_libdir}/Nemerle.dll || :

%files
%defattr(644,root,root,755)
%doc AUTHORS README doc/html/*
%attr(755,root,root) %{_bindir}/ncc
%attr(755,root,root) %{_bindir}/ncc.exe
%attr(755,root,root) %{_libdir}/Nemerle.Macros.dll
%attr(755,root,root) %{_libdir}/Nemerle.Compiler.dll
%{_mandir}/man1/*
%ghost %attr(755,root,root) %{_bindir}/ncc.exe.so
%ghost %attr(755,root,root) %{_libdir}/Nemerle.Macros.dll.so
%ghost %attr(755,root,root) %{_libdir}/Nemerle.Compiler.dll.so

%files libs
%defattr(644,root,root,755)
%doc COPYRIGHT
%attr(755,root,root) %{_libdir}/Nemerle.dll
%ghost %attr(755,root,root) %{_libdir}/Nemerle.dll.so

%define date	%(echo `LC_ALL="C" date +"%a %b %d %Y"`)
%changelog
* %{date} PLD Team <feedback@pld-linux.org>
All persons listed below can be reached at <cvs_login>@pld-linux.org

$Log: nemerle.spec,v $
Revision 1.5  2004/02/04 14:22:32  malekith
- up to 0.0.1592

Revision 1.4  2004/01/30 23:30:08  malekith
- upgrade to 0.0.1522

Revision 1.3  2004/01/26 19:41:55  malekith
- -libs also requires mono

Revision 1.2  2004/01/26 12:42:38  malekith
- nemerle r: nemerle-libs

Revision 1.1  2004/01/26 12:32:09  malekith
- initial
