<TeXmacs|1.0.2.5>

<style|seminar>

<\body>
  <\with|left margin|>
    <\make-title>
      <title|NEMERLE>

      \;

      <\with|font size|1.19>
        (Not) Yet Another\ 

        Functional <verbatim|.NET> Language\ 

        \;
      </with>

      <author|Michaª Moskal>

      University of Wrocªaw

      \;

      \;

      \;

      \;

      \;

      \;

      \;

      \;

      \;

      \;

      \;

      <with|font size|1.19|<with|color|blue|<verbatim|ht<with|color|blue|>tp://nemerle.org/>><with|font
      size|1.41|>>
    </make-title>
  </with>

  <\with|paragraph mode|left>
    <section|Reasons -- why new language?>

    <\itemize>
      <item><verbatim|.NET> (with generics) provides best environment for
      high level, statically typed languages we have seen so
      far<apply|set-footer|>

      <item>we wanted a functional language (yes, this is a matter of taste)

      <item>ports of existing functional languages to <verbatim|.NET> did not
      provide full integration

      <item>we wanted a language that is easy to learn

      <\itemize-minus>
        <item>easy access to object-oriented and imperative constructs

        <item>functional programming can be introduced slowly, step by step

        <item>familiar syntax that does not scare
      </itemize-minus>
    </itemize>

    \;

    <\with|paragraph mode|center>
      <\with|font size|1.19>
        <subsection|<with|font size|1.41|And last but not least>>
      </with>

      <\with|font size|1.41>
        <with|font series|medium|<strong|Designing programming languages is a
        lot of <em|<with|color|black|fun>>> :-)>
      </with>
    </with>

    <new_page>

    <section|Method -- how to do it?>

    <subsubsection|Combine>

    <\itemize>
      <item><verbatim|.NET> object-oriented type system with generics

      <item>ML datatypes <with|font shape|italic|aka> variants, that allow
      inheriting variants from objects

      <item>ML-like expressions with:

      <\itemize-minus>
        <item>pattern matching

        <item>higher order functions

        <item>simple bottom-up type synthesis
      </itemize-minus>
    </itemize>

    <section|And the Result>

    <\itemize>
      <item>full <verbatim|.NET> interoperability

      <\itemize-minus>
        <item>CLS code can be easily produced and consumed

        <item>full support for overloading
      </itemize-minus>

      <item>integration of functional, object-oriented, and imperative
      constructs

      <item>OO system is a one--to--one mapping of CLS and thus is easy to
      learn

      <item>static typing with on-demand dynamic types
    </itemize>

    <new_page>

    <section|Hello world><subsubsection|>

    <\itemize>
      <item>top-level program structure is similar to <verbatim|C#>

      <item>core library (providing <verbatim|print_string> function) borrows
      interfaces from OCaml
    </itemize>

    <\with|font size|0.84>
      <\code>
        <strong|<em| \ \ \ \ \ \ class>> Hello {

        \ \ \ \ \ \ \ \ \ <em|public> <em|static> Main () : <em|void> {

        \ \ \ \ \ \ \ \ \ \ \ print_string ("Hello world!\\n");

        \ \ \ \ \ \ \ \ \ }

        \ \ \ \ \ \ \ }
      </code>
    </with>

    <section|Factorial>

    <\itemize>
      <item>the simple way to write factorial function
    </itemize>

    <with|font size|0.84|<\code>
      \ \ \ \ \ \ \ <em|static> <em|public> factorial (x :
      <with|color|brown|int>) : <with|color|brown|int> {

      \ \ \ \ \ \ \ \ \ <em|if> (x \<less\>= 1) \ 1

      \ \ \ \ \ \ \ \ \ <em|else> \ \ \ \ \ \ \ \ x * factorial (x - 1)

      \ \ \ \ \ \ \ }
    </code>>

    <\itemize>
      <item>and now with local function and tail recursion, which is as
      efficient as <verbatim|while> loop in <verbatim|C#>
    </itemize>

    <with|font size|0.84|<\code>
      \ \ \ \ \ \ \ <em|static> <em|public> factorial (x :
      <with|color|brown|int>) : <with|color|brown|int> {

      \ \ \ \ \ \ \ \ \ <em|def> loop (acc : <with|color|brown|int>, x :
      <with|color|brown|int>) : <with|color|brown|int> {

      \ \ \ \ \ \ \ \ \ \ \ <em|if> (x \<less\>= 1) \ acc

      \ \ \ \ \ \ \ \ \ \ \ <em|else> \ \ \ \ \ \ \ \ loop (x * acc, x - 1)

      \ \ \ \ \ \ \ \ \ }

      \ \ \ \ \ \ \ \ \ loop (1, x)

      \ \ \ \ \ \ \ }
    </code>>

    <new_page><new_page>

    <section|Variants>

    <\itemize>
      <item>variants provide simple yet powerful method of performing
      symbolic computations

      <item>list and tree are classic examples
    </itemize>

    <\with|font size|0.84>
      <\code>
        <em| \ \ \ \ \ \ variant> <with|color|brown|list ('a)> {

        \ \ \ \ \ \ \ \ \ \| Cons { hd : <with|color|brown|'a>; tl
        :<with|color|brown| list ('a)>; }

        \ \ \ \ \ \ \ \ \ \| Nil

        \ \ \ \ \ \ \ }
      </code>
    </with>

    <\itemize>
      <item>variants are de-constructed with <verbatim|<em|match>> expression
    </itemize>

    <\with|font size|0.84>
      <\code>
        \ \ \ \ \ \ \ <em|><with|color|brown|'a> head (l :
        <with|color|brown|list ('a)>) : <with|color|brown|'a> {

        \ \ \ \ \ \ \ \ \ \ <em|match> (l) {<em|>

        \ \ \ \ \ \ \ \ \ \ \ \ \| Cons (x, _) =\<gtr\> x

        \ \ \ \ \ \ \ \ \ \ \ \ \| Nil =\<gtr\>\ 

        \ \ \ \ \ \ \ \ \ \ \ \ \ \ <em|raise> Invalid_argument ("List.head")

        \ \ \ \ \ \ \ \ \ \ }

        \ \ \ \ \ \ \ }
      </code>
    </with>

    <\itemize>
      <item>search trees require elements to provide a comparison function
    </itemize>

    <with|font size|0.84|<\code>
      \ \ \ \ \ \ \ <em|variant> <with|color|brown|tree ('a)>\ 

      \ \ \ \ \ \ \ \ \ <em|where> <with|color|brown|'a> :\<gtr\>
      <with|color|brown|IComparable ('a)> {

      \ \ \ \ \ \ \ \ \ \ \ \| Node { l : <with|color|brown|tree ('a)>; d :
      <with|color|brown|'a>;\ 

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ r : <with|color|brown|tree
      ('a)>; }

      \ \ \ \ \ \ \ \ \ \ \ \| Tip

      \ \ \ \ \ \ \ }
    </code>>

    <new_page>

    <section|Variant inheritance>

    <\itemize>
      <item>unlike in ML variants can have common part expressed in terms of
      inheritance

      <item>this is used in Nemerle compiler <em|(written in Nemerle itself)>
      <emdash> common part of all expressions is the originating source code
      location
    </itemize>

    <subsubsection|Example>

    <\code>
      <\with|font size|0.84>
        <em|class> <with|color|brown|Located> {

        \ \ file : <with|color|brown|string>;

        \ \ line : <with|color|brown|int>;

        }

        \;

        <em|variant> <with|color|brown|Expr> <em|extends>
        <with|color|brown|Located> {

        \ \ \| E_call { fn : <with|color|brown|Expr>; parms :
        <with|color|brown|list (Expr)>; }

        \ \ \| E_ref { name : <with|color|brown|string>; }

        \ \ \| E_int_literal { value : <with|color|brown|int>; }

        }

        \;

        <em|public> <em|static> dump (e : <with|color|brown|Expr>) :
        <with|color|brown|void> {

        \ \ print ("// " + e.file + ": " + itoa (e.line));

        \ \ <em|match> (e) {

        \ \ \ \ \| E_int_literal (i) =\<gtr\> print_int (i)

        \ \ \ \ \| E_ref r =\<gtr\> print (r.name)

        \ \ \ \ \| E_call c =\<gtr\>\ 

        \ \ \ \ \ \ dump (c.fn);\ 

        \ \ \ \ \ \ List.iter (dump, c.parms);

        \ \ }

        }
      </with>
    </code>

    <section|Functional values>

    <\itemize>
      <item><strong|functions are first class citizens> <emdash> can be
      passed as arguments and returned as results from other functions

      <item>special syntax for lists:

      <\itemize-minus>
        <item><verbatim|<with|color|red|x> :: <with|color|red|xs>> is
        <verbatim|Cons (<with|color|red|x>, <with|color|red|xs>)>

        <item><verbatim|[]> is <verbatim|Nil ()>

        <item><verbatim|@> is list concatentation operator
      </itemize-minus>
    </itemize>

    <\with|font size|0.84>
      <\code>
        <with|color|brown|'a> split (l : <with|color|brown|list ('a)>, r :
        <with|color|brown|list ('a)>,\ 

        \ \ \ \ \ \ \ \ \ \ cmp : <with|color|brown|'a * 'a -\<gtr\> int>, m
        : <with|color|brown|'a>,\ 

        \ \ \ \ \ \ \ \ \ \ inp : <with|color|brown|list ('a)>) :
        <with|color|brown|list ('a)> {

        \ \ <em|match> (inp) {

        \ \ \ \ \| x :: xs =\<gtr\>

        \ \ \ \ \ \ <em|if> (cmp (m, x) \<gtr\> 0) split (y :: l, r, xs)

        \ \ \ \ \ \ <em|else> split (l, x :: r, xs)

        \ \ \ \ \| [] =\<gtr\> (l, r)

        \ \ }

        }

        \;

        <with|color|brown|'a> qsort (cmp : <with|color|brown|'a * 'a -\<gtr\>
        int>,\ 

        \ \ \ \ \ \ \ \ \ \ inp : <with|color|brown|list ('a)>) :
        <with|color|brown|list ('a)> {

        \ \ <em|match> (inp) {

        \ \ \ \ \| x :: xs =\<gtr\>

        \ \ \ \ \ \ <em|def> (l, r) = split ([], [], cmp, x, xs);

        \ \ \ \ \ \ qsort (cmp, l) @ (x :: qsort (cmp, r))

        \ \ \ \ \| [] =\<gtr\> []

        \ \ }

        }
      </code>
    </with>

    <new_page>

    <section|Formal semantics>

    <\itemize>
      <item>formal semantics of the language will be described using term
      rewriting systems

      <item>existing equational theorem provers (<verbatim|Isabelle>,
      <verbatim|VeriFun>) can be used to prove program properties
    </itemize>

    <section|Assertions>

    <\itemize>
      <item><verbatim|<em|require>> at the beginning of a block or function

      <item><verbatim|<em|ensure>> at the end that can use
      <verbatim|<em|value>> keyword (value of the block)

      <with|font size|0.84|<\code>
        <em|static> <em|public> factorial (x : <with|color|brown|int>) :
        <with|color|brown|int> {

        \ \ <em|require> { x \<gtr\>= 1 }

        \;

        \ \ <em|def> loop (acc : <with|color|brown|int>, x :
        <with|color|brown|int>) : <with|color|brown|int> {

        \ \ \ \ <em|require> { x \<gtr\>= 1 }

        \ \ \ \ <em|if> (x == 1) \ acc

        \ \ \ \ <em|else> \ \ \ \ \ \ \ \ loop (x * acc, x - 1)

        \ \ }

        \;

        \ \ loop (1, x);

        \ \ <em|ensure> { <em|value> \<gtr\>= x;

        \ \ \ \ \ \ \ \ \ \ \ x \<gtr\>= 3 == (<em|value> % 3 == 0) }

        }
      </code>>

      <item>assertions will help in automatic test data generation for
      programs
    </itemize>

    <new_page>

    <section|Assertions for mutables>

    <\itemize>
      <item><em|<verbatim|guarded>> mutable values

      <\itemize-minus>
        <item>update of guarded value triggers assertion check

        <item>can use special <verbatim|<em|previous>> qualifier to refer to
        the value before update

        <item>can be defined for all mutable values (class fields and locals)
      </itemize-minus>

      <item><em|<verbatim|guard>> assertions

      <\itemize-minus>
        <item>checked at the definition point

        <item>checked at update of each directly referenced mutable value

        <item>propagated up the call stack for fields

        <item>checked after calls for locals (in case locals are passed by
        <verbatim|ref>)
      </itemize-minus>

      <item><verbatim|<em|transaction>> block

      <\itemize-minus>
        <item>guards are stacked and executed at the end of
        <verbatim|<em|<em|transaction>>> block

        <item>allow assertions like <verbatim|x + y == 5>
      </itemize-minus>

      \;
    </itemize>

    <new_page>

    <section|Powerful, type safe macros>

    <\itemize>
      <item>users can easily write macros, that are in fact compiler plugins

      <item>plugins take expression or type trees as input

      <item>plugins can execute any code (including accessing external files)

      <item>plugins return expression or type trees
    </itemize>

    \;

    <subsubsection|Combine>

    <\itemize>
      <item>power of Lisp code-generating macros

      <item>static type safety
    </itemize>

    \;

    <subsubsection|Example uses>

    <\itemize>
      <item>type-safe <verbatim|printf>

      <item><verbatim|$>--interpolation, <with|font shape|italic|a'la> Bourne
      shell or Perl

      <item>convenient syntax for regular expressions

      <item>automatic generation of XML serialization methods for types

      <item>automatic generation of types from SQL tables or <with|font
      shape|italic|vice versa>

      <item>binding results of SQL queries to source language variables in a
      type-safe way
    </itemize>

    <new_page>

    <section|Regular expression macro>

    <\with|font size|0.84>
      <\code>
        parse_time (t : <with|color|brown|string>) : <with|color|brown|int>

        \ \ {

        \ \ \ \ <with|color|green|<with|color|magenta|rxmatch>> (t,
        <with|color|magenta|"([0-9]+:<with|color|red|hour>):([0-9]+:<with|color|red|minute>)">);

        \ \ \ \ (hour * 60 + minute)

        \ \ }
      </code>
    </with>

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ <postscript|<tuple|<raw_data|%!PS-Adobe-2.0
    EPSF-2.0\n%%Title: arrow-small.dia\n%%Creator: Dia
    v0.92-pre5\n%%CreationDate: Tue Nov \ 4 23:54:30 2003\n%%For:
    malekith\n%%Orientation: Portrait\n%%Magnification:
    1.0000\n%%BoundingBox: 0 0 126 60\n%%BeginSetup\n%%EndSetup\n%%EndComments\n%%BeginProlog\n[
    /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
    /.notdef /.notdef\n/.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
    /.notdef /.notdef /.notdef /.notdef\n/.notdef /.notdef /.notdef /.notdef
    /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef\n/.notdef /.notdef
    /space /exclam /quotedbl /numbersign /dollar /percent /ampersand
    /quoteright\n/parenleft /parenright /asterisk /plus /comma /hyphen
    /period /slash /zero /one\n/two /three /four /five /six /seven /eight
    /nine /colon /semicolon\n/less /equal /greater /question /at /A /B /C /D
    /E\n/F /G /H /I /J /K /L /M /N /O\n/P /Q /R /S /T /U /V /W /X /Y\n/Z
    /bracketleft /backslash /bracketright /asciicircum /underscore /quoteleft
    /a /b /c\n/d /e /f /g /h /i /j /k /l /m\n/n /o /p /q /r /s /t /u /v
    /w\n/x /y /z /braceleft /bar /braceright /asciitilde /.notdef /.notdef
    /.notdef\n/.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
    /.notdef /.notdef /.notdef\n/.notdef /.notdef /.notdef /.notdef /.notdef
    /.notdef /.notdef /.notdef /.notdef /.notdef\n/.notdef /.notdef /.notdef
    /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef\n/space
    /exclamdown /cent /sterling /currency /yen /brokenbar /section /dieresis
    /copyright\n/ordfeminine /guillemotleft /logicalnot /hyphen /registered
    /macron /degree /plusminus /twosuperior /threesuperior\n/acute /mu
    /paragraph /periodcentered /cedilla /onesuperior /ordmasculine
    /guillemotright /onequarter /onehalf\n/threequarters /questiondown
    /Agrave /Aacute /Acircumflex /Atilde /Adieresis /Aring /AE
    /Ccedilla\n/Egrave /Eacute /Ecircumflex /Edieresis /Igrave /Iacute
    /Icircumflex /Idieresis /Eth /Ntilde\n/Ograve /Oacute /Ocircumflex
    /Otilde /Odieresis /multiply /Oslash /Ugrave /Uacute
    /Ucircumflex\n/Udieresis /Yacute /Thorn /germandbls /agrave /aacute
    /acircumflex /atilde /adieresis /aring\n/ae /ccedilla /egrave /eacute
    /ecircumflex /edieresis /igrave /iacute /icircumflex /idieresis\n/eth
    /ntilde /ograve /oacute /ocircumflex /otilde /odieresis /divide /oslash
    /ugrave\n/uacute /ucircumflex /udieresis /yacute /thorn /ydieresis]
    /isolatin1encoding exch def\n/cp {closepath} bind def\n/c {curveto} bind
    def\n/f {fill} bind def\n/a {arc} bind def\n/ef {eofill} bind def\n/ex
    {exch} bind def\n/gr {grestore} bind def\n/gs {gsave} bind def\n/sa
    {save} bind def\n/rs {restore} bind def\n/l {lineto} bind def\n/m
    {moveto} bind def\n/rm {rmoveto} bind def\n/n {newpath} bind def\n/s
    {stroke} bind def\n/sh {show} bind def\n/slc {setlinecap} bind def\n/slj
    {setlinejoin} bind def\n/slw {setlinewidth} bind def\n/srgb {setrgbcolor}
    bind def\n/rot {rotate} bind def\n/sc {scale} bind def\n/sd {setdash}
    bind def\n/ff {findfont} bind def\n/sf {setfont} bind def\n/scf
    {scalefont} bind def\n/sw {stringwidth pop} bind def\n/tr {translate}
    bind def\n\n/ellipsedict 8 dict def\nellipsedict /mtrx matrix
    put\n/ellipse\n{ ellipsedict begin\n \ \ /endangle exch def\n
    \ \ /startangle exch def\n \ \ /yrad exch def\n \ \ /xrad exch def\n
    \ \ /y exch def\n \ \ /x exch def \ \ /savematrix mtrx currentmatrix
    def\n \ \ x y tr xrad yrad sc\n \ \ 0 0 1 startangle endangle arc\n
    \ \ savematrix setmatrix\n \ \ end\n} def\n\n/mergeprocs {\ndup length\n3
    -1 roll\ndup\nlength\ndup\n5 1 roll\n3 -1 roll\nadd\narray cvx\ndup\n3 -1
    roll\n0 exch\nputinterval\ndup\n4 2 roll\nputinterval\n} bind def\n/dpi_x
    300 def\n/dpi_y 300 def\n/conicto {\n \ \ \ /to_y exch def\n \ \ \ /to_x
    exch def\n \ \ \ /conic_cntrl_y exch def\n \ \ \ /conic_cntrl_x exch
    def\n \ \ \ currentpoint\n \ \ \ /p0_y exch def\n \ \ \ /p0_x exch def\n
    \ \ \ /p1_x p0_x conic_cntrl_x p0_x sub 2 3 div mul add def\n \ \ \ /p1_y
    p0_y conic_cntrl_y p0_y sub 2 3 div mul add def\n \ \ \ /p2_x p1_x to_x
    p0_x sub 1 3 div mul add def\n \ \ \ /p2_y p1_y to_y p0_y sub 1 3 div mul
    add def\n \ \ \ p1_x p1_y p2_x p2_y to_x to_y curveto\n} bind
    def\n/start_ol { gsave 1.1 dpi_x div dup scale} bind def\n/end_ol {
    closepath fill grestore } bind def\n28.346000 -28.346000
    scale\n-14.788197 -11.055902 translate\n%%EndProlog\n\n\n0.100000 slw\n[]
    0 sd\n[] 0 sd\n0 slj\n0 slc\n0.917647 0.917647 0.917647 srgb\nn 16.000000
    9.000000 m 18.000000 9.000000 l 18.000000 10.000000 l 19.000000 10.000000
    l 17.000000 11.000000 l 15.000000 10.000000 l 16.000000 10.000000 l
    f\n0.000000 0.000000 0.000000 srgb\nn 16.000000 9.000000 m 18.000000
    9.000000 l 18.000000 10.000000 l 19.000000 10.000000 l 17.000000
    11.000000 l 15.000000 10.000000 l 16.000000 10.000000 l cp
    s\nshowpage\n>|eps>||||||>

    <\with|font size|0.84>
      <\code>
        parse_time (t : <with|color|brown|string>) : <with|color|brown|int>

        \ \ {

        \ \ \ \ <with|color|black|<em|def>
        <with|color|magenta|<with|color|black|r =>>> Regexp
        (<with|color|magenta|"([0-9]+):([0-9]+)">);

        \ \ \ \ <em|def> m = r.Matches (t);

        \ \ \ \ <with|color|magenta|<with|color|magenta|<with|color|black|<em|def>>
        <with|color|red|hour><with|color|black| = r [0];>>>

        \ \ \ \ <with|color|magenta|<with|color|black|<em|def>>
        <with|color|red|minute> <with|color|black|= r [1];>>

        \ \ \ \ (hour * 60 + minute)

        \ \ }

        \;
      </code>
    </with>

    <subsubsection|Remarks>

    <\itemize>
      <item><verbatim|<with|color|magenta|rxmatch>> <emdash> a special
      function that triggers plugin execution

      <item>plugin produces bindings for named groups

      <item>standard <verbatim|.NET> regular expression function is called on
      regular expression with names removed
    </itemize>

    <new_page>

    <section|SQL queries macro>

    <with|font size|0.84|<\code>
      <with|font size|0.84|>sql_loop (conn,\ 

      \ \ \ \ \ \ \ \ \ \ <with|color|magenta|"SELECT
      <with|color|red|salary>, LOWER (name) AS <with|color|red|lname>>

      \ \ \ \ \ \ \ \ \ \ \ \ \ <with|color|magenta|FROM employees >

      \ \ \ \ \ \ \ \ \ \ \ \ \ <with|color|magenta|WHERE salary \<gtr\>
      <with|mode|math|><with|mode|math|>$(<with|color|red|min_salary * 3>)">,

      \ \ \ \ \ \ \ \ \ \ print_string (lname + ":" +\ 

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ itoa (salary)))
    </code>>

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ <postscript|<tuple|<raw_data|%!PS-Adobe-2.0
    EPSF-2.0\n%%Title: arrow-small.dia\n%%Creator: Dia
    v0.92-pre5\n%%CreationDate: Tue Nov \ 4 23:54:30 2003\n%%For:
    malekith\n%%Orientation: Portrait\n%%Magnification:
    1.0000\n%%BoundingBox: 0 0 126 60\n%%BeginSetup\n%%EndSetup\n%%EndComments\n%%BeginProlog\n[
    /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
    /.notdef /.notdef\n/.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
    /.notdef /.notdef /.notdef /.notdef\n/.notdef /.notdef /.notdef /.notdef
    /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef\n/.notdef /.notdef
    /space /exclam /quotedbl /numbersign /dollar /percent /ampersand
    /quoteright\n/parenleft /parenright /asterisk /plus /comma /hyphen
    /period /slash /zero /one\n/two /three /four /five /six /seven /eight
    /nine /colon /semicolon\n/less /equal /greater /question /at /A /B /C /D
    /E\n/F /G /H /I /J /K /L /M /N /O\n/P /Q /R /S /T /U /V /W /X /Y\n/Z
    /bracketleft /backslash /bracketright /asciicircum /underscore /quoteleft
    /a /b /c\n/d /e /f /g /h /i /j /k /l /m\n/n /o /p /q /r /s /t /u /v
    /w\n/x /y /z /braceleft /bar /braceright /asciitilde /.notdef /.notdef
    /.notdef\n/.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
    /.notdef /.notdef /.notdef\n/.notdef /.notdef /.notdef /.notdef /.notdef
    /.notdef /.notdef /.notdef /.notdef /.notdef\n/.notdef /.notdef /.notdef
    /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef\n/space
    /exclamdown /cent /sterling /currency /yen /brokenbar /section /dieresis
    /copyright\n/ordfeminine /guillemotleft /logicalnot /hyphen /registered
    /macron /degree /plusminus /twosuperior /threesuperior\n/acute /mu
    /paragraph /periodcentered /cedilla /onesuperior /ordmasculine
    /guillemotright /onequarter /onehalf\n/threequarters /questiondown
    /Agrave /Aacute /Acircumflex /Atilde /Adieresis /Aring /AE
    /Ccedilla\n/Egrave /Eacute /Ecircumflex /Edieresis /Igrave /Iacute
    /Icircumflex /Idieresis /Eth /Ntilde\n/Ograve /Oacute /Ocircumflex
    /Otilde /Odieresis /multiply /Oslash /Ugrave /Uacute
    /Ucircumflex\n/Udieresis /Yacute /Thorn /germandbls /agrave /aacute
    /acircumflex /atilde /adieresis /aring\n/ae /ccedilla /egrave /eacute
    /ecircumflex /edieresis /igrave /iacute /icircumflex /idieresis\n/eth
    /ntilde /ograve /oacute /ocircumflex /otilde /odieresis /divide /oslash
    /ugrave\n/uacute /ucircumflex /udieresis /yacute /thorn /ydieresis]
    /isolatin1encoding exch def\n/cp {closepath} bind def\n/c {curveto} bind
    def\n/f {fill} bind def\n/a {arc} bind def\n/ef {eofill} bind def\n/ex
    {exch} bind def\n/gr {grestore} bind def\n/gs {gsave} bind def\n/sa
    {save} bind def\n/rs {restore} bind def\n/l {lineto} bind def\n/m
    {moveto} bind def\n/rm {rmoveto} bind def\n/n {newpath} bind def\n/s
    {stroke} bind def\n/sh {show} bind def\n/slc {setlinecap} bind def\n/slj
    {setlinejoin} bind def\n/slw {setlinewidth} bind def\n/srgb {setrgbcolor}
    bind def\n/rot {rotate} bind def\n/sc {scale} bind def\n/sd {setdash}
    bind def\n/ff {findfont} bind def\n/sf {setfont} bind def\n/scf
    {scalefont} bind def\n/sw {stringwidth pop} bind def\n/tr {translate}
    bind def\n\n/ellipsedict 8 dict def\nellipsedict /mtrx matrix
    put\n/ellipse\n{ ellipsedict begin\n \ \ /endangle exch def\n
    \ \ /startangle exch def\n \ \ /yrad exch def\n \ \ /xrad exch def\n
    \ \ /y exch def\n \ \ /x exch def \ \ /savematrix mtrx currentmatrix
    def\n \ \ x y tr xrad yrad sc\n \ \ 0 0 1 startangle endangle arc\n
    \ \ savematrix setmatrix\n \ \ end\n} def\n\n/mergeprocs {\ndup length\n3
    -1 roll\ndup\nlength\ndup\n5 1 roll\n3 -1 roll\nadd\narray cvx\ndup\n3 -1
    roll\n0 exch\nputinterval\ndup\n4 2 roll\nputinterval\n} bind def\n/dpi_x
    300 def\n/dpi_y 300 def\n/conicto {\n \ \ \ /to_y exch def\n \ \ \ /to_x
    exch def\n \ \ \ /conic_cntrl_y exch def\n \ \ \ /conic_cntrl_x exch
    def\n \ \ \ currentpoint\n \ \ \ /p0_y exch def\n \ \ \ /p0_x exch def\n
    \ \ \ /p1_x p0_x conic_cntrl_x p0_x sub 2 3 div mul add def\n \ \ \ /p1_y
    p0_y conic_cntrl_y p0_y sub 2 3 div mul add def\n \ \ \ /p2_x p1_x to_x
    p0_x sub 1 3 div mul add def\n \ \ \ /p2_y p1_y to_y p0_y sub 1 3 div mul
    add def\n \ \ \ p1_x p1_y p2_x p2_y to_x to_y curveto\n} bind
    def\n/start_ol { gsave 1.1 dpi_x div dup scale} bind def\n/end_ol {
    closepath fill grestore } bind def\n28.346000 -28.346000
    scale\n-14.788197 -11.055902 translate\n%%EndProlog\n\n\n0.100000 slw\n[]
    0 sd\n[] 0 sd\n0 slj\n0 slc\n0.917647 0.917647 0.917647 srgb\nn 16.000000
    9.000000 m 18.000000 9.000000 l 18.000000 10.000000 l 19.000000 10.000000
    l 17.000000 11.000000 l 15.000000 10.000000 l 16.000000 10.000000 l
    f\n0.000000 0.000000 0.000000 srgb\nn 16.000000 9.000000 m 18.000000
    9.000000 l 18.000000 10.000000 l 19.000000 10.000000 l 17.000000
    11.000000 l 15.000000 10.000000 l 16.000000 10.000000 l cp
    s\nshowpage\n>|eps>||||||>

    <with|font size|0.84|<\code>
      <with|font size|0.84|><em|def> cmd = SqlCommand
      (<with|color|magenta|"SELECT salary, LOWER (name)>

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ <with|color|magenta| \ \ \ FROM
      employees>\ 

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ <with|color|magenta|WHERE
      salary \<gtr\> <with|color|red|@parm1>">,\ 

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ conn);

      cmd.Parameters.Add (<with|color|magenta|"<with|color|red|@parm1>">,
      <with|color|red|min_salary * 3>);

      <em|def<em|>> r = cmd.ExecuteReader ();

      <em|while> (r.Read ()) {

      \ \ <em|def> <with|color|red|salary> = r.GetInt32 (0);

      \ \ <em|def> <with|color|red|lname> = r.GetString (1);

      \ \ print_string (lname + ": " + itoa (salary))

      }
    </code>>

    <subsubsection|Remarks>

    <\itemize>
      <item>this macro requires SQL parser aware of types of SQL functions

      <item>but it knows and uses the correct type of
      <verbatim|<with|color|red|salary>> and
      <verbatim|<with|color|red|lname>> variables
    </itemize>
  </with>

  <new_page>

  <section|Real--life example>

  Small part of Nemerle compiler, so you can see how the language ``feels''.

  <\code>
    <\with|font size|0.84>
      string_of_type (t : <with|color|brown|Type>) :
      <with|color|brown|string> {

      \ \ <em|def> map (sep : <with|color|brown|string>,\ 

      \ \ \ \ \ \ \ \ \ \ \ args : <with|color|brown|list (Type)>) :
      <with|color|brown|string> {

      \ \ \ \ Util.concat_strings (sep,\ 

      \ \ \ \ \ \ \ \ List.map (string_of_type, args))

      \ \ }

      \ \ <em|match> (t) {

      \ \ \ \ \| T_app (ti, args) =\<gtr\>

      \ \ \ \ \ \ <em|def> name = ti.fullname ();

      \ \ \ \ \ \ <em|match> (args) {

      \ \ \ \ \ \ \ \ \| Nil =\<gtr\> name

      \ \ \ \ \ \ \ \ \| _ =\<gtr\>\ 

      \ \ \ \ \ \ \ \ \ \ \ name + <with|color|magenta|" ("> + map
      (<with|color|magenta|", ">, args) + <with|color|magenta|")">

      \ \ \ \ \ \ }

      \ \ \ \ \| T_var (tv) =\<gtr\>\ 

      \ \ \ \ \ \ <with|color|magenta|"'"> + tv.name +
      <with|color|magenta|"_"> +\ 

      \ \ \ \ \ \ string_of_int (tv.id) +\ 

      \ \ \ \ \ \ <em|if> (Tyvar.is_free (tv)) <with|color|magenta|"*">
      <em|else> <with|color|magenta|"">

      \ \ \ \ \| T_ref (t) =\<gtr\> <with|color|magenta|"ref "> +
      string_of_type (t)

      \ \ \ \ \| T_out (t) =\<gtr\> <with|color|magenta|"out "> +
      string_of_type (t)

      \ \ \ \ \| T_void =\<gtr\> <with|color|magenta|"void">

      \ \ \ \ \| T_prod (args) =\<gtr\>\ 

      \ \ \ \ \ \ <with|color|magenta|"("> + map (<with|color|magenta|" * ">,
      args) + <with|color|magenta|")">

      \ \ \ \ \| T_fun (from, to) =\<gtr\>

      \ \ \ \ \ \ <with|color|magenta|"("> + string_of_type (from) +
      <with|color|magenta|" -\<gtr\> "> +

      \ \ \ \ \ \ \ string_of_type (to) + <with|color|magenta|")">

      \ \ }

      }
    </with>
  </code>
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|shrinking factor|10>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
    <associate|background color|white>
    <associate|language|english>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|toc-20|<tuple|<uninit>|11>>
    <associate|toc-10|<tuple|<uninit>|6>>
    <associate|gly-1|<tuple|1|?>>
    <associate|toc-11|<tuple|<uninit>|6>>
    <associate|toc-21|<tuple|<uninit>|12>>
    <associate|toc-12|<tuple|<uninit>|7>>
    <associate|toc-22|<tuple|<uninit>|12>>
    <associate|toc-23|<tuple|<uninit>|13>>
    <associate|toc-13|<tuple|<uninit>|8>>
    <associate|toc-24|<tuple|<uninit>|14>>
    <associate|toc-14|<tuple|<uninit>|8>>
    <associate|toc-15|<tuple|<uninit>|9>>
    <associate|toc-16|<tuple|<uninit>|10>>
    <associate|toc-17|<tuple|<uninit>|10>>
    <associate|toc-18|<tuple|<uninit>|10>>
    <associate|toc-19|<tuple|<uninit>|11>>
    <associate|toc-1|<tuple|<uninit>|2>>
    <associate|toc-2|<tuple|<uninit>|2>>
    <associate|toc-3|<tuple|<uninit>|3>>
    <associate|toc-4|<tuple|<uninit>|3>>
    <associate|toc-5|<tuple|<uninit>|3>>
    <associate|toc-6|<tuple|<uninit>|4>>
    <associate|toc-7|<tuple|<uninit>|4>>
    <associate|toc-8|<tuple|<uninit>|4>>
    <associate|toc-9|<tuple|<uninit>|5>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      Reasons -- why new language?<value|toc-dots><pageref|toc-1>

      <with|left margin|<quote|1.5fn>|<with|font size|<quote|1.41>|And last
      but not least><value|toc-dots><pageref|toc-2>>

      Method -- how to do it?<value|toc-dots><pageref|toc-3>

      <with|left margin|<quote|3fn>|Combine<value|toc-dots><pageref|toc-4>>

      And the Result<value|toc-dots><pageref|toc-5>

      Hello world<value|toc-dots><pageref|toc-6>

      <with|left margin|<quote|3fn>|<value|toc-dots><pageref|toc-7>>

      Factorial<value|toc-dots><pageref|toc-8>

      Variants<value|toc-dots><pageref|toc-9>

      Variant inheritance<value|toc-dots><pageref|toc-10>

      <with|left margin|<quote|3fn>|Example<value|toc-dots><pageref|toc-11>>

      Functional values<value|toc-dots><pageref|toc-12>

      Formal semantics<value|toc-dots><pageref|toc-13>

      Assertions<value|toc-dots><pageref|toc-14>

      Assertions for mutables<value|toc-dots><pageref|toc-15>

      Powerful, type safe macros<value|toc-dots><pageref|toc-16>

      <with|left margin|<quote|3fn>|Combine<value|toc-dots><pageref|toc-17>>

      <with|left margin|<quote|3fn>|Example
      uses<value|toc-dots><pageref|toc-18>>

      Regular expression macro<value|toc-dots><pageref|toc-19>

      <with|left margin|<quote|3fn>|Remarks<value|toc-dots><pageref|toc-20>>

      SQL queries macro<value|toc-dots><pageref|toc-21>

      <with|left margin|<quote|3fn>|Remarks<value|toc-dots><pageref|toc-22>>

      Real--life example<value|toc-dots><pageref|toc-23>
    </associate>
  </collection>
</auxiliary>