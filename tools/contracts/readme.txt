The Nemerle.Contracts library allows to perform a static (that is at 
a compile time) verification of programs. For this, the Spec# compiler 
is required (available at http://research.microsoft.com/specsharp/) and 
the Simplify automatic theorem prover (how to get Simplify please see
http://research.microsoft.com/specsharp/simplify.htm).
The Spec# compiler requires Microsoft Visual Studio 2005 to be installed.

For more information about this project please see:
www.ii.uni.wroc.pl/~wwa/msc.pdf

Compile the Nemerle.Contracts library as follows:

ncc -t:dll Nemerle.Contracts.n -o Nemerle.Contracts.dll -r:Nemerle.Compiler.dll -r:(SPEC#)\System.Compiler.dll

where (SPEC#) is a path to Spec# compiler.
To compile a program with contracts, only a reference to Nemerle.Contracts
is required, so the compilation goes as follows:

ncc (PROGRAM).n -r:Nemerle.Contracts.dll

where (PROGRAM) is a source code file.

