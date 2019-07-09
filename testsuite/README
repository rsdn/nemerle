 _____________________________________________________________________________
|     test.n - Utillity for compile and runtime testing of Nemerle programs   |
|     /* In Nemerle project used for regressive tests */                      |
 -----------------------------------------------------------------------------
FIXME - no clue how to write it in a really good way ;(
  RUNING:
            The only thing it needs to be run is being compiled by Nemerle compilator.
              Example command: ncc test.n -texe -out:test.exe

  USAGE:
            test.exe {PARAMETERS} {FILES}

  DESCRIPTION:
    test.n will pass through a list of FILES (if no files passed then it takes
    all files *.n in current directory) and determines whether required 
    compile and runtime conditions (defined by metadata contained in file code 
    see: CODE METADATA TAGS) are met. If not it prints to stdout and test_error.log 
    file which conditions in which files have not been satisfied. 
     
    PARAMETERS are one of following:
      -ncc FILE, -n FILE          - use this FILE as Nemerle compiler (default: ncc.exe)
      -reference FILE, -ref FILE  - pass this FILE as reference to Nemerle compiler
                                    during compilation of each tested file (default: none)
      -include FILE, -inc FILE, -i FILE - pass this FILE as Nemerle include file 
                                          during compilation of each tested file (default: none)
      -runtime ENGINE, -r ENGINE  - use ENGINE as a .NET runtime engine (default: none)
      -verbose, -v                - prints all Nemerle output (default: no)
      -vv                         - prints all Nemerle and runtime output (default: no)
            
  CODE METADATA TAGS:
    CODE METADATA TAGS are contained in file code (should be commented out), being one of following
    and define one of corresponding conditions:
    // E: <regexp> - placed at the end of line defines: expect an error in this line 
                     which description can be matched by <regexp> 
    // W: <regexp> - placed at the end of line defines: expect a warning in this line 
                     which description can be matched by <regexp>
    // OK          - placed at the end of line defines: expect no error in this line
    BEGIN-OUTPUT
    <output 1>
    ...
    <output n>
    END-OUTPUT     - placed anywhere in code provided that each line starts from begining of
                     current line (FIXME: it's a bit unclear) defines:
                     Program should compile to exe file successfully and its runtime output
                     is to match (verbatim) text contained between BEGIN-OUTPUT and END-OUTPUT
    BEGIN-INPUT
    <input 1>
    ...
    <input n>
    END-INPUT      - placed anywhere in code provided that each line starts from begining of
                     current line (FIXME: it's a bit unclear) defines:
                     Program should compile successfully and during its runtime
                     text contained between BEGIN-INPUT and END-INPUT will be passed to stdin
    // NO-TEST     - placed at the end of any line defines: do not test this file
    // REFERENCE : FILE 
                   - placed at the end of any line defines: pass this FILE as reference library
                     to nemerle compiler during this file compilation
    // INCLUDE : FILE 
                   - placed at the end of any line defines: pass this FILE as include file
                     to nemerle compiler during this file compilation

  ADDITIONAL NOTES:
    - if tested file contains no BEGIN-OUTPUT tag then it is compiled to an dll (library) file
