This is implementation of C# 5.0's async/await keywords for Nemerle 1.1 and .NET 4.0.

Features:
- compatible with C# 5.0 Task-based Asynchronous Pattern
- syntax resembles that of C#
- .NET 4.5 and VS2012 are not required
  (works fine with .NET 4.0 and VS2010 with AsyncTargetingPack.NET4)
- support for the following language constructs:
  do / while / for / foreach / try / catch / finally / using
- support for GetAwaiter
- support for ConfigureAwait
- support for IProgress
- support for CancellationToken

Before use, please ensure that you know exactly what
"Task-based Asynchronous Pattern" is about:
http://www.microsoft.com/en-us/download/details.aspx?id=19957

AsyncTargetingPack.NET4 is required when using with .NET 4.0:
http://www.microsoft.com/en-us/download/details.aspx?id=29576
Reference to AsyncTargetingPack can be removed in .NET 4.5 or later.

For more info please see here:
http://sites.google.com/site/gibekm/programming/nemerle/asyncawait
  
Marek Gibek
