Installation
    I assume that you installed #develop 3.2 into it's default directory:
    %ProgramFiles%\SharpDevelop\3.0
    If so, just start INSTALL.CMD with administrative permissions, it will
    compile Nemerle language binding directly to #develop addins directory.

    Otherwise when you installed SharpDevelop into custom path you have to
    create SharpDevelop30 environment variable with SharpDevelop path, like this:
    X:\DevTools\SharpDevelop\3.0
    and run BUILD.CMD with administrative permissions.

    If you want to manualy edit *.nproj files with full XML parsing support you 
    have to edit configuration file of XmlEditor addin:
    %SharpDevelop30%\AddIns\AddIns\DisplayBindings\XmlEditor\XmlEditor.addin
    Find 'supportedextensions' attribute in 'Parser' tag and add '.nproj'.
    That's all.


Developing addin
    If you want to debug and modify the addin you have to:
    1) download #develop 3.2 sources from here:
        http://www.sharpdevelop.net/OpenSource/SD/Download/
    2) extract files to any directory (I use C:\!Proj\SharpDevelop\3.2)
    3) define SharpDevelop30 environment variable with that one
    4) build #develop (you can use existing #develop installation).
    So you can build and run binding under debugger!


                                                             // hc, 25.04.2010