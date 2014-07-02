function ParseArguments(prefix) {
  var data = Session.Property("CustomActionData").split("|");
  return {
    PkgDefFilePath : data[1] + "\\Nemerle.VisualStudio.pkgdef",
    NemerlePath    : data[0] + "\\Net-4.0"
  };
}

function GetPkgDefFile(args) {
  var fso  = new ActiveXObject("Scripting.FileSystemObject");

  var x = fso.CreateTextFile("c:\\pkgdefpatcher.log");
  x.WriteLine("PkgDefFilePath  = " + args.PkgDefFilePath);
  x.WriteLine("NemerlePath     = " + args.NemerlePath);
  x.Close();

  var file = fso.GetFile(args.PkgDefFilePath);
  return file;
}

function ReadPkgDefFile(file) {
  var readStream = pkgdefFile.OpenAsTextStream(1, -2);
  var text = readStream.ReadAll();
  readStream.Close();
  return text;
}

function SubstitutePaths(args, text) {
  var newText = text.replace(/\$PackageFolder\$\\Nemerle\.VisualStudio\.dll/gi, args.NemerlePath + "\\Nemerle.VisualStudio.dll");
  return newText;
}

function VS2010() {
  var args        = ParseArguments("VS2010");
  var pkgdefFile  = GetPkgDefFile(args);
  var text        = ReadPkgDefFile(pkgdefFile);
  var newText     = SubstitutePaths(args, text);
  var writeStream = pkgdefFile.OpenFileAsTextStream(2, -2);
  writeStream.Write(newText);
  writeStream.Close();
}

function VS2012() {
  var args        = ParseArguments("VS2012");
  var pkgdefFile  = GetPkgDefFile(args);
  var text        = ReadPkgDefFile(pkgdefFile);
  var newText     = SubstitutePaths(args, text);
  var writeStream = pkgdefFile.OpenFileAsTextStream(2, -2);
  writeStream.Write(newText);
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
  writeStream.Close();
}

function VS2013() {
  var args        = ParseArguments("VS2013");
  var pkgdefFile  = GetPkgDefFile(args);
  var text        = ReadPkgDefFile(pkgdefFile);
  var newText     = SubstitutePaths(args, text);
  var writeStream = pkgdefFile.OpenFileAsTextStream(2, -2);
  writeStream.Write(newText);
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
  writeStream.Close();
}
