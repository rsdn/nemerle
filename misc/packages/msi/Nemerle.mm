;----------------------------------------------------------------------------
;    MODULE NAME:   Nemerle.mm
;
;        $Author:   USER "dave@nemerle.org"  $
;      $Revision:   0.1  $
;


#define? DEPT_SUPPORT_WEB_URL       http://nemerle.org/
#define? DEPT_NAME_SHORT            Nemerle Project
#define? DEPT_NAME                  The Nemerle Project Team
#define? DEPT_ADDRESS               Poland
#define? DEPT_MSI_MANUFACTURER      <$DEPT_NAME>
#define? DEPT_MSI_AUTHOR            <$DEPT_NAME>
#define? COMPANY_CONTACT_NAME       University of Wroclaw
#define? COMPANY_CONTACT_NAME_PHONE 
#define? COMPANY_DOCO_RELATIVE_DIR  Nemerle Project
#define? COMPANY_LICENCE_SPELLING_C_OR_S  s
#define? ONEXIT_GENERATE_HTML	N

#define? COMPANY_PACKAGED_BY        Packaged by <$DEPT_NAME> (<$DEPT_ADDRESS>).
;--- Load MAKEMSI (via wrapper) ---------------------------------------------
#include "DEPT.MMH"

;--- Create INSTALLDIR ------------------------------------------------------

<$DirectoryTree Key="INSTALLDIR" Dir="[ProgramFilesFolder]\Nemerle" CHANGE="\">

<$Feature "compiler_and_libs" Title="Compiler and libraries" Create="Y">
 <$Component "MainFiles" Create="Y" Directory_="INSTALLDIR">
  <$Files "dist\bin\*.exe" DestDir="INSTALLDIR">
  <$Files "dist\bin\*.dll" DestDir="INSTALLDIR">
  <$Files "dist\License.rtf" DestDir="INSTALLDIR">
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


