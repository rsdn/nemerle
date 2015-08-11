function ParseArguments(prefix) {
  var data = Session.Property("CustomActionData").split("|");
  return {
    PkgDefFilePath             : data[1] + "Nemerle.VisualStudio.pkgdef",
    NemerlePath                : data[0] + "Net-4.0\\",
    NemerleVisualStudioVersion : data[2]
  };
}

function GetPkgDefFile(args) {
  var fso  = new ActiveXObject("Scripting.FileSystemObject");
  var file = fso.GetFile(args.PkgDefFilePath);
  return file;
}

function ReadPkgDefFile(pkgdefFile) {
  var readStream = pkgdefFile.OpenAsTextStream(1, -2);
  var text = readStream.ReadAll();
  readStream.Close();
  return text;
}

function SubstitutePaths(args, text) {
  var newText = text.replace(/\$PackageFolder\$\\Nemerle\.VisualStudio\.dll/gi, args.NemerlePath + "Nemerle.VisualStudio.dll");
  return newText;
}

function VS2010() {
  if (!Session) return;

  var args        = ParseArguments("VS2010");
  var pkgdefFile  = GetPkgDefFile(args);
  var text        = ReadPkgDefFile(pkgdefFile);
  var newText     = SubstitutePaths(args, text);
  var writeStream = pkgdefFile.OpenAsTextStream(2, -2);
  writeStream.Write(newText);

  if (newText.indexOf('[$RootKey$\\RuntimeConfiguration\\dependentAssembly\\bindingRedirection\\{FE369FD8-C6DB-433B-907B-05A115708416}]') < 0) {
    writeStream.WriteLine('[$RootKey$\\RuntimeConfiguration\\dependentAssembly\\bindingRedirection\\{FE369FD8-C6DB-433B-907B-05A115708416}]');
    writeStream.WriteLine('"name"="Nemerle.VisualStudio"'                                                                                   );
    writeStream.WriteLine('"publicKeyToken"="c4c0f22444bf4088"'                                                                             );
    writeStream.WriteLine('"culture"="neutral"'                                                                                             );
    writeStream.WriteLine('"oldVersion"="1.0.0.0-' + args.NemerleVisualStudioVersion + '"'                                                  );
    writeStream.WriteLine('"newVersion"="' + args.NemerleVisualStudioVersion + '"'                                                          );
  }

  writeStream.Close();
}

function VS2012() {
  if (!Session) return;

  var args        = ParseArguments("VS2012");
  var pkgdefFile  = GetPkgDefFile(args);
  var text        = ReadPkgDefFile(pkgdefFile);
  var newText     = SubstitutePaths(args, text);
  var writeStream = pkgdefFile.OpenAsTextStream(2, -2);
  writeStream.Write(newText);

  if (newText.indexOf('[$RootKey$\\RuntimeConfiguration\\dependentAssembly\\bindingRedirection\\{FE369FD8-C6DB-433B-907B-05A115708416}]') < 0) {
    writeStream.WriteLine('[$RootKey$\\RuntimeConfiguration\\dependentAssembly\\bindingRedirection\\{FE369FD8-C6DB-433B-907B-05A115708416}]');
    writeStream.WriteLine('"name"="Nemerle.VisualStudio"'                                                                                   );
    writeStream.WriteLine('"publicKeyToken"="c4c0f22444bf4088"'                                                                             );
    writeStream.WriteLine('"culture"="neutral"'                                                                                             );
    writeStream.WriteLine('"oldVersion"="1.0.0.0-' + args.NemerleVisualStudioVersion + '"'                                                  );
    writeStream.WriteLine('"newVersion"="' + args.NemerleVisualStudioVersion + '"'                                                          );
  }

  if (newText.indexOf('[$RootKey$\\RuntimeConfiguration\\dependentAssembly\\bindingRedirection\\{BD1D3C51-E157-4DE0-A535-E94130D1970A}]') < 0) {
    writeStream.WriteLine('[$RootKey$\\RuntimeConfiguration\\dependentAssembly\\bindingRedirection\\{BD1D3C51-E157-4DE0-A535-E94130D1970A}]');
    writeStream.WriteLine('"name"="Microsoft.Windows.Design.Host"'                                                                          );
    writeStream.WriteLine('"publicKeyToken"="b03f5f7f11d50a3a"'                                                                             );
    writeStream.WriteLine('"culture"="neutral"'                                                                                             );
    writeStream.WriteLine('"oldVersion"="4.0.0.0"'                                                                                          );
    writeStream.WriteLine('"newVersion"="4.1.0.0"'                                                                                          );
  }

  if (newText.indexOf('[$RootKey$\\RuntimeConfiguration\\dependentAssembly\\bindingRedirection\\{5978995F-FBA0-4DCC-8556-19AB2EB19D36}]') < 0) {
    writeStream.WriteLine('[$RootKey$\\RuntimeConfiguration\\dependentAssembly\\bindingRedirection\\{5978995F-FBA0-4DCC-8556-19AB2EB19D36}]');
    writeStream.WriteLine('"name"="Microsoft.Windows.Design.Interaction"'                                                                   );
    writeStream.WriteLine('"publicKeyToken"="b03f5f7f11d50a3a"'                                                                             );
    writeStream.WriteLine('"culture"="neutral"'                                                                                             );
    writeStream.WriteLine('"oldVersion"="4.0.0.0"'                                                                                          );
    writeStream.WriteLine('"newVersion"="4.1.0.0"'                                                                                          );
  }

  if (newText.indexOf('[$RootKey$\\RuntimeConfiguration\\dependentAssembly\\bindingRedirection\\{F604D514-4F33-422E-BA44-E4F22381F044}]') < 0) {
    writeStream.WriteLine('[$RootKey$\\RuntimeConfiguration\\dependentAssembly\\bindingRedirection\\{F604D514-4F33-422E-BA44-E4F22381F044}]');
    writeStream.WriteLine('"name"="Microsoft.VisualStudio.Web.Application"'                                                                 );
    writeStream.WriteLine('"publicKeyToken"="b03f5f7f11d50a3a"'                                                                             );
    writeStream.WriteLine('"culture"="neutral"'                                                                                             );
    writeStream.WriteLine('"oldVersion"="10.0.0.0"'                                                                                         );
    writeStream.WriteLine('"newVersion"="11.0.0.0"'                                                                                         );
  }

  writeStream.Close();
}

function VS2013() {
  if (!Session) return;

  var args        = ParseArguments("VS2013");
  var pkgdefFile  = GetPkgDefFile(args);
  var text        = ReadPkgDefFile(pkgdefFile);
  var newText     = SubstitutePaths(args, text);
  var writeStream = pkgdefFile.OpenAsTextStream(2, -2);
  writeStream.Write(newText);

  if (newText.indexOf('[$RootKey$\\RuntimeConfiguration\\dependentAssembly\\bindingRedirection\\{FE369FD8-C6DB-433B-907B-05A115708416}]') < 0) {
    writeStream.WriteLine('[$RootKey$\\RuntimeConfiguration\\dependentAssembly\\bindingRedirection\\{FE369FD8-C6DB-433B-907B-05A115708416}]');
    writeStream.WriteLine('"name"="Nemerle.VisualStudio"'                                                                                   );
    writeStream.WriteLine('"publicKeyToken"="c4c0f22444bf4088"'                                                                             );
    writeStream.WriteLine('"culture"="neutral"'                                                                                             );
    writeStream.WriteLine('"oldVersion"="1.0.0.0-' + args.NemerleVisualStudioVersion + '"'                                                  );
    writeStream.WriteLine('"newVersion"="' + args.NemerleVisualStudioVersion + '"'                                                          );
  }

  if (newText.indexOf('[$RootKey$\\RuntimeConfiguration\\dependentAssembly\\bindingRedirection\\{BD1D3C51-E157-4DE0-A535-E94130D1970A}]') < 0) {
    writeStream.WriteLine('[$RootKey$\\RuntimeConfiguration\\dependentAssembly\\bindingRedirection\\{BD1D3C51-E157-4DE0-A535-E94130D1970A}]');
    writeStream.WriteLine('"name"="Microsoft.Windows.Design.Host"'                                                                          );
    writeStream.WriteLine('"publicKeyToken"="b03f5f7f11d50a3a"'                                                                             );
    writeStream.WriteLine('"culture"="neutral"'                                                                                             );
    writeStream.WriteLine('"oldVersion"="4.0.0.0-4.1.0.0"'                                                                                  );
    writeStream.WriteLine('"newVersion"="4.2.0.0"'                                                                                          );
  }

  if (newText.indexOf('[$RootKey$\\RuntimeConfiguration\\dependentAssembly\\bindingRedirection\\{5978995F-FBA0-4DCC-8556-19AB2EB19D36}]') < 0) {
    writeStream.WriteLine('[$RootKey$\\RuntimeConfiguration\\dependentAssembly\\bindingRedirection\\{5978995F-FBA0-4DCC-8556-19AB2EB19D36}]');
    writeStream.WriteLine('"name"="Microsoft.Windows.Design.Interaction"'                                                                   );
    writeStream.WriteLine('"publicKeyToken"="b03f5f7f11d50a3a"'                                                                             );
    writeStream.WriteLine('"culture"="neutral"'                                                                                             );
    writeStream.WriteLine('"oldVersion"="4.0.0.0-4.1.0.0"'                                                                                  );
    writeStream.WriteLine('"newVersion"="4.2.0.0"'                                                                                          );
  }

  if (newText.indexOf('[$RootKey$\\RuntimeConfiguration\\dependentAssembly\\bindingRedirection\\{F604D514-4F33-422E-BA44-E4F22381F044}]') < 0) {
    writeStream.WriteLine('[$RootKey$\\RuntimeConfiguration\\dependentAssembly\\bindingRedirection\\{F604D514-4F33-422E-BA44-E4F22381F044}]');
    writeStream.WriteLine('"name"="Microsoft.VisualStudio.Web.Application"'                                                                 );
    writeStream.WriteLine('"publicKeyToken"="b03f5f7f11d50a3a"'                                                                             );
    writeStream.WriteLine('"culture"="neutral"'                                                                                             );
    writeStream.WriteLine('"oldVersion"="10.0.0.0-11.0.0.0"'                                                                                );
    writeStream.WriteLine('"newVersion"="12.0.0.0"'                                                                                         );
  }

  writeStream.Close();
}

function VS2015() {
  if (!Session) return;

  var args        = ParseArguments("VS2015");
  var pkgdefFile  = GetPkgDefFile(args);
  var text        = ReadPkgDefFile(pkgdefFile);
  var newText     = SubstitutePaths(args, text);
  var writeStream = pkgdefFile.OpenAsTextStream(2, -2);
  writeStream.Write(newText);

  if (newText.indexOf('[$RootKey$\\RuntimeConfiguration\\dependentAssembly\\bindingRedirection\\{FE369FD8-C6DB-433B-907B-05A115708416}]') < 0) {
    writeStream.WriteLine('[$RootKey$\\RuntimeConfiguration\\dependentAssembly\\bindingRedirection\\{FE369FD8-C6DB-433B-907B-05A115708416}]');
    writeStream.WriteLine('"name"="Nemerle.VisualStudio"'                                                                                   );
    writeStream.WriteLine('"publicKeyToken"="c4c0f22444bf4088"'                                                                             );
    writeStream.WriteLine('"culture"="neutral"'                                                                                             );
    writeStream.WriteLine('"oldVersion"="1.0.0.0-' + args.NemerleVisualStudioVersion + '"'                                                  );
    writeStream.WriteLine('"newVersion"="' + args.NemerleVisualStudioVersion + '"'                                                          );
  }

  if (newText.indexOf('[$RootKey$\\RuntimeConfiguration\\dependentAssembly\\bindingRedirection\\{BD1D3C51-E157-4DE0-A535-E94130D1970A}]') < 0) {
    writeStream.WriteLine('[$RootKey$\\RuntimeConfiguration\\dependentAssembly\\bindingRedirection\\{BD1D3C51-E157-4DE0-A535-E94130D1970A}]');
    writeStream.WriteLine('"name"="Microsoft.Windows.Design.Host"'                                                                          );
    writeStream.WriteLine('"publicKeyToken"="b03f5f7f11d50a3a"'                                                                             );
    writeStream.WriteLine('"culture"="neutral"'                                                                                             );
    writeStream.WriteLine('"oldVersion"="4.0.0.0-4.2.0.0"'                                                                                  );
    writeStream.WriteLine('"newVersion"="4.3.0.0"'                                                                                          );
  }

  if (newText.indexOf('[$RootKey$\\RuntimeConfiguration\\dependentAssembly\\bindingRedirection\\{5978995F-FBA0-4DCC-8556-19AB2EB19D36}]') < 0) {
    writeStream.WriteLine('[$RootKey$\\RuntimeConfiguration\\dependentAssembly\\bindingRedirection\\{5978995F-FBA0-4DCC-8556-19AB2EB19D36}]');
    writeStream.WriteLine('"name"="Microsoft.Windows.Design.Interaction"'                                                                   );
    writeStream.WriteLine('"publicKeyToken"="b03f5f7f11d50a3a"'                                                                             );
    writeStream.WriteLine('"culture"="neutral"'                                                                                             );
    writeStream.WriteLine('"oldVersion"="4.0.0.0-4.2.0.0"'                                                                                  );
    writeStream.WriteLine('"newVersion"="4.3.0.0"'                                                                                          );
  }

  if (newText.indexOf('[$RootKey$\\RuntimeConfiguration\\dependentAssembly\\bindingRedirection\\{F604D514-4F33-422E-BA44-E4F22381F044}]') < 0) {
    writeStream.WriteLine('[$RootKey$\\RuntimeConfiguration\\dependentAssembly\\bindingRedirection\\{F604D514-4F33-422E-BA44-E4F22381F044}]');
    writeStream.WriteLine('"name"="Microsoft.VisualStudio.Web.Application"'                                                                 );
    writeStream.WriteLine('"publicKeyToken"="b03f5f7f11d50a3a"'                                                                             );
    writeStream.WriteLine('"culture"="neutral"'                                                                                             );
    writeStream.WriteLine('"oldVersion"="10.0.0.0-12.0.0.0"'                                                                                );
    writeStream.WriteLine('"newVersion"="14.0.0.0"'                                                                                         );
  }

  writeStream.Close();
}

function UpdateFileTimestamp() {
  var filePath   = Session.Property("CustomActionData");
  var fso        = new ActiveXObject("Scripting.FileSystemObject");
  var fileStream = fso.OpenTextFile(filePath, /*ForWrite*/ 2,  /*CreateNew*/ true, /*TristateFalse*/ 0);
  fileStream.Write("It's never too late to have a happy childhood.");
  fileStream.Close();
}
