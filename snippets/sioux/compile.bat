del *.dll
del *.exe
del *.pdb
copy ..\..\boot\*.dll .
..\..\boot\ncc.exe -tdll -r:System.Xml -out:Nemerle.Xml.dll xmltemplate.n
..\..\boot\ncc.exe -tdll -r:System.Xml -r:System.Web -r:Nemerle.Xml -out:Sioux.dll config.n logger.n httpd.n request.n response.n application.n
..\..\boot\ncc.exe -tdll -r:System.Xml -r:System.Web -r:Nemerle.Xml -r:Sioux -out:Sioux.Fit.dll fit\fit.n fit\submission.n
..\..\boot\ncc.exe -texe -r:System.Xml -r:System.Web -r:Sioux -r:Sioux.Fit -out:httpd.exe sioux.n
