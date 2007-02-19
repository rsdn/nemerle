IF exist positive.results del positive.results
tests.exe -d:positive -p "-nowarn:10003 -def:RUNTIME_MS" -s > positive.results
type positive.results
echo ----------------------------------------
pause