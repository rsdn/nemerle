SET MSBUILDENABLEALLPROPERTYFUNCTIONS=1
SET Config=Debug
rem msbuild NemerleAll.nproj /t:Stage1;CompilerTests;_ComputationExpressions;_Async;Install /p:NTargetName=Build;Configuration=%Config%;NInstall=c:\RSDN\nemerle\bin\%Config%\net-4.0
msbuild NemerleAll.nproj /t:DevBuildQuick /p:NTargetName=Build;Configuration=%Config%;NInstall=c:\RSDN\nemerle\bin\%Config%\net-4.0
pause
