IF exist negative.results del negative.results
tests.exe -d:negative -p "-nowarn:10003 -def:RUNTIME_MS" -s > negative.results
type negative.results
echo ----------------------------------------
pause