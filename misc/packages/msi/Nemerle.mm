;----------------------------------------------------------------------------
;    MODULE NAME:   Nemerle.mm
;
;        $Author:   USER "dave@nemerle.org"  $
;      $Revision:   0.1  $
;


;--- Load MAKEMSI (via wrapper) ---------------------------------------------
#include "DEPT.MMH"

;--- Create INSTALLDIR ------------------------------------------------------

<$DirectoryTree Key="INSTALLDIR" Dir="[ProgramFilesFolder]\Nemerle" CHANGE="\">

<$Feature "compiler_and_libs" Title="Compiler and libraries" Create="Y">
 <$Component "MainFiles" Create="Y" Directory_="INSTALLDIR">
  <$Files "dist\bin\*.exe" DestDir="INSTALLDIR">
  <$Files "dist\bin\*.dll" DestDir="INSTALLDIR">
  <$Files "dist\Licence.rtf" DestDir="INSTALLDIR">
  <$Path   "[INSTALLDIR]">
 <$/Component>
<$/Feature>

<$Feature "documentation" Title="Documentation" Create="Y">
  <$Files "dist\html\*" Component="" SubDir="TREE" DestDir="[INSTALLDIR]\html">
  <$Component "DocLinks" Create="Y" Directory_="INSTALLDIR">
    <$DirectoryTree Key="SHORTCUTDIR" Dir="[ProgramMenuFolder]\Nemerle" Make="Y" REMOVE=Y">

    #(  
        <$Shortcut
                   Dir="SHORTCUTDIR"
               Target="[INSTALLDIR]html\index.html"
                 Title="Nemerle Documentation"
        >
    #)
    #(
       <$Shortcut
                   Dir="SHORTCUTDIR"
                Target="http://www.nemerle.org/"
                 Title="Nemerle homepage"
           Description="Go Nemerle homepage"
       >
    #)

  <$/Component>

<$/Feature>


