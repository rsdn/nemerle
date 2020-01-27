SET MSBUILDENABLEALLPROPERTYFUNCTIONS=1
SET Config=Release
msbuild NemerleAll.nproj /t:Stage1;CompilerTests;_ComputationExpressions;_Async;Install /p:NTargetName=Build;Configuration=%Config%;NInstall=c:\RSDN\nemerle\bin\%Config%\net-4.0
pause
