IF exist negative.results del negative.results
tests.exe -d:negative -p "-nowarn:10003 -def:RUNTIME_MS" -s
rem > negative.results
rem type negative.results
echo ----------------------------------------
pause