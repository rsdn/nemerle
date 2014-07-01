var ErrorCodes = {
  PkgdefFileNotFound : 1,
  InsuffisientNumberOfArguments : 2,
  InvalidVisualStudioVersion : 3
};

var OpenFileReasons = {
  Read : 1,
  Write : 2
};


if (WScript.Arguments.length < 3) {
  WScript.Quit(ErrorCodes.InsuffisientNumberOfArguments);
}

var visualStudioVersion = WScript.Arguments(0);
var pkgdefFilePath      = WScript.Arguments(1);
var nemerleDir          = WScript.Arguments(2);

var fso = new ActiveXObject("Scripting.FileSystemObject");
if (!fso.FileExists(pkgdefFilePath)) {
  WScript.Quit(ErrorCodes.PkgdefFileNotFound);
}

var pkgdefFile = fso.GetFile(pkgdefFilePath);


var readStream = pkgdefFile.OpenAsTextStream(OpenFileReasons.Read, -2);
var text = readStream.ReadAll();
readStream.Close();


var newText = text.replace(/\$PackageFolder\$\\Nemerle\.VisualStudio\.dll/gi, nemerleDir + "\\Nemerle.VisualStudio.dll");


var writeStream = pkgdefFile.OpenAsTextStream(OpenFileReasons.Write, -2);
switch(visualStudioVersion)
{
  case "VS2010":
    writeStream.Write(newText);
    break;

  case "VS2012":
    writeStream.WriteLine(newText);
    writeStream.WriteLine('[$RootKey$\\RuntimeConfiguration\\dependentAssembly\\bindingRedirection\\{BD1D3C51-E157-4DE0-A535-E94130D1970A}]');
    writeStream.WriteLine('"name"="Microsoft.Windows.Design.Host"'                                                                          );
    writeStream.WriteLine('"publicKeyToken"="b03f5f7f11d50a3a"'                                                                             );
    writeStream.WriteLine('"culture"="neutral"'                                                                                             );
    writeStream.WriteLine('"oldVersion"="4.0.0.0"'                                                                                          );
    writeStream.WriteLine('"newVersion"="4.1.0.0"'                                                                                          );
    writeStream.WriteLine('[$RootKey$\\RuntimeConfiguration\\dependentAssembly\\bindingRedirection\\{5978995F-FBA0-4DCC-8556-19AB2EB19D36}]');
    writeStream.WriteLine('"name"="Microsoft.Windows.Design.Interaction"'                                                                   );
    writeStream.WriteLine('"publicKeyToken"="b03f5f7f11d50a3a"'                                                                             );
    writeStream.WriteLine('"culture"="neutral"'                                                                                             );
    writeStream.WriteLine('"oldVersion"="4.0.0.0"'                                                                                          );
    writeStream.WriteLine('"newVersion"="4.1.0.0"'                                                                                          );
    writeStream.WriteLine('[$RootKey$\\RuntimeConfiguration\\dependentAssembly\\bindingRedirection\\{F604D514-4F33-422E-BA44-E4F22381F044}]');
    writeStream.WriteLine('"name"="Microsoft.VisualStudio.Web.Application"'                                                                 );
    writeStream.WriteLine('"publicKeyToken"="b03f5f7f11d50a3a"'                                                                             );
    writeStream.WriteLine('"culture"="neutral"'                                                                                             );
    writeStream.WriteLine('"oldVersion"="10.0.0.0"'                                                                                         );
    writeStream.WriteLine('"newVersion"="11.0.0.0"'                                                                                         );
    break;

  case "VS2013":
    writeStream.WriteLine(newText);
    writeStream.WriteLine('[$RootKey$\\RuntimeConfiguration\\dependentAssembly\\bindingRedirection\\{BD1D3C51-E157-4DE0-A535-E94130D1970A}]');
    writeStream.WriteLine('"name"="Microsoft.Windows.Design.Host"'                                                                          );
    writeStream.WriteLine('"publicKeyToken"="b03f5f7f11d50a3a"'                                                                             );
    writeStream.WriteLine('"culture"="neutral"'                                                                                             );
    writeStream.WriteLine('"oldVersion"="4.0.0.0-4.1.0.0"'                                                                                  );
    writeStream.WriteLine('"newVersion"="4.2.0.0"'                                                                                          );
    writeStream.WriteLine('[$RootKey$\\RuntimeConfiguration\\dependentAssembly\\bindingRedirection\\{5978995F-FBA0-4DCC-8556-19AB2EB19D36}]');
    writeStream.WriteLine('"name"="Microsoft.Windows.Design.Interaction"'                                                                   );
    writeStream.WriteLine('"publicKeyToken"="b03f5f7f11d50a3a"'                                                                             );
    writeStream.WriteLine('"culture"="neutral"'                                                                                             );
    writeStream.WriteLine('"oldVersion"="4.0.0.0-4.1.0.0"'                                                                                  );
    writeStream.WriteLine('"newVersion"="4.2.0.0"'                                                                                          );
    writeStream.WriteLine('[$RootKey$\\RuntimeConfiguration\\dependentAssembly\\bindingRedirection\\{F604D514-4F33-422E-BA44-E4F22381F044}]');
    writeStream.WriteLine('"name"="Microsoft.VisualStudio.Web.Application"'                                                                 );
    writeStream.WriteLine('"publicKeyToken"="b03f5f7f11d50a3a"'                                                                             );
    writeStream.WriteLine('"culture"="neutral"'                                                                                             );
    writeStream.WriteLine('"oldVersion"="10.0.0.0-11.0.0.0"'                                                                                );
    writeStream.WriteLine('"newVersion"="12.0.0.0"'                                                                                         );
    break;

  default:
    WScript.Quit(ErrorCodes.InvalidVisualStudioVersion);
    break;
}
writeStream.Close();
