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

      <with|color|blue|<verbatim|ht<with|color|blue|>tp://nemerle.org/>>
    </make-title>

    \;
  </with>

  <section|Reasons -- why new language?>

  <\itemize>
    <item><verbatim|.NET> (with generics) provides best environment for high
    level, statically typed languages we have seen so far

    <item>we wanted functional language (yes, this is a matter of taste)

    <item>ports of existing functional languages to <verbatim|.NET> did not
    provide full integration

    <item>we wanted language that is easy to learn

    <\itemize-minus>
      <item>easy access to object-oriented and imperative constructs

      <item>functional programming can be introduced slowly, step by step

      <item>familiar syntax that does not scare
    </itemize-minus>
  </itemize>

  \;

  <with|font size|1.19|<\with|paragraph mode|center>
    <subsection|<with|font size|1.41|And the most important reason>>
  </with>>

  <with|font size|1.41|<\with|paragraph mode|center>
    <with|font series|medium|<strong|Designing programming languages is a lot
    of <em|<with|color|black|fun>>> :-)>
  </with>>

  <new_page>

  <section|Method -- how to do it?>

  <subsubsection|Combine>

  <\itemize>
    <item><verbatim|.NET> object-oriented type system with generics

    <item>ML datatypes <with|font shape|italic|aka> variants, allow
    inheriting variants from objects (have common part in all variants)

    <item>ML-like expression core with:

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

    <item>OO system is 1:1 mapping of CLS and thus easy to learn

    <item>static typing with on-demand dynamic types
  </itemize>

  <new_page>

  <section|Formal semantics>

  <\itemize>
    <item>formal semantics of the language will be described using term
    rewriting systems

    <item>existing equational theorem provers (<verbatim|Isabelle>,
    <verbatim|VeriFun>) can be used to prove program properties

    \;
  </itemize>

  \;

  <section|Assertions and tests>

  Assertions in code will help in:

  <\itemize>
    <item>automatic test data generation

    <item>proving program properties at TRS level

    <item>design by contract
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
    \ \ \ \ \ \ \ <em|static> <em|public> fact (x : <with|color|brown|int>) :
    <with|color|brown|int> {

    \ \ \ \ \ \ \ \ \ <em|if> (x \<less\>= 1) \ 1

    \ \ \ \ \ \ \ \ \ <em|else> \ \ \ \ \ \ \ \ x * fact (x - 1)

    \ \ \ \ \ \ \ }
  </code>>

  <\itemize>
    <item>and now with local function and tail recursion, which is as
    efficient as <verbatim|while> loop in <verbatim|C#>
  </itemize>

  <with|font size|0.84|<\code>
    \ \ \ \ \ \ \ <em|static> <em|public> fact (x : <with|color|brown|int>) :
    <with|color|brown|int> {

    \ \ \ \ \ \ \ \ \ <em|def> loop (acc : <with|color|brown|int>, x :
    <with|color|brown|int>) {

    \ \ \ \ \ \ \ \ \ \ \ <em|if> (x \<less\>= 1) \ acc

    \ \ \ \ \ \ \ \ \ \ \ <em|else> \ \ \ \ \ \ \ \ loop (x * acc, x - 1)

    \ \ \ \ \ \ \ \ \ }

    \ \ \ \ \ \ \ \ \ loop (1, x)

    \ \ \ \ \ \ \ }
  </code>>

  <new_page>

  <section|Variants>

  <\itemize>
    <item>variants provide simple yet powerful method of performing symbolic
    computations

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
    <item>unlike list tree requires elements to provide comparison function
  </itemize>

  <with|font size|0.84|<\code>
    \ \ \ \ \ \ \ <em|variant> <with|color|brown|tree ('a)>\ 

    \ \ \ \ \ \ \ \ \ <em|where> <with|color|brown|'a> :\<gtr\>
    <with|color|brown|IComparable ('a)> {

    \ \ \ \ \ \ \ \ \ \ \ \| Node { l : <with|color|brown|tree ('a)>; d :
    <with|color|brown|'a>;\ 

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ r : <with|color|brown|tree ('a)>;
    }

    \ \ \ \ \ \ \ \ \ \ \ \| Tip

    \ \ \ \ \ \ \ }
  </code>>

  <section|Functional values>

  <\itemize>
    <item><strong|functions are first class citizens> <emdash> can be passed
    as arguments and returned as results from other functions
  </itemize>

  <\with|font size|0.84>
    <\code>
      \ \ <with|color|brown| \ \ \ \ 'a>, <with|color|brown|'b> map (f :
      <with|color|brown|'a -\<gtr\> 'b>, x : <with|color|brown|list ('a)>)\ 

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ : <with|color|brown|list ('b)> {

      \ \ \ \ \ \ \ \ \ <em|match> (x) {

      \ \ \ \ \ \ \ \ \ \ \ \| Nil =\<gtr\> Nil ()

      \ \ \ \ \ \ \ \ \ \ \ \| Cons x =\<gtr\>\ 

      \ \ \ \ \ \ \ \ \ \ \ \ \ Cons (f (x.hd), map (f, x.tl))

      \ \ \ \ \ \ \ \ \ }

      \ \ \ \ \ \ \ }
    </code>
  </with>

  <section|Binary search tree insertion>

  <with|font size|0.84|<\code>
    \ \ <with|color|brown|'a> <em|where> <with|color|brown|'a> :\<gtr\>
    <with|color|brown|IComparable ('a)>\ 

    \ \ \ \ insert (x : <with|color|brown|t ('a)>, e : <with|color|brown|'a>)
    : <with|color|brown|t ('a)> {

    \ \ \ \ <em|match> (x) {

    \ \ \ \ \ \ \| Node (l, d, r) =\<gtr\>

    \ \ \ \ \ \ \ \ <em|def> res = d.Compare (e);

    \ \ \ \ \ \ \ \ <em|if> (res \<less\> 0)\ 

    \ \ \ \ \ \ \ \ \ \ Node (insert (l, e), d, r)

    \ \ \ \ \ \ \ \ <em|else if> (res \<gtr\> 0)

    \ \ \ \ \ \ \ \ \ \ Node (l, d, insert (r, e))

    \ \ \ \ \ \ \ \ <em|else>\ 

    \ \ \ \ \ \ \ \ \ \ <em|raise> Invalid_argument ("insert")

    \ \ \ \ \ \ \| Tip =\<gtr\> Node (Tip (), e, Tip ())

    \ \ \ \ }

    \ \ }
  </code>>

  <section|Safe, Turing-complete macros>

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

    \;
  </with>

  \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ <postscript|<tuple|<raw_data|%!PS-Adobe-2.0
  EPSF-2.0\n%%Title: arrow.dia\n%%Creator: Dia v0.92-pre5\n%%CreationDate:
  Tue Nov \ 4 23:23:14 2003\n%%For: malekith\n%%Orientation:
  Portrait\n%%Magnification: 1.0000\n%%BoundingBox: 0 0 121
  117\n%%BeginSetup\n%%EndSetup\n%%EndComments\n%%BeginProlog\n[ /.notdef
  /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
  /.notdef\n/.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
  /.notdef /.notdef /.notdef\n/.notdef /.notdef /.notdef /.notdef /.notdef
  /.notdef /.notdef /.notdef /.notdef /.notdef\n/.notdef /.notdef /space
  /exclam /quotedbl /numbersign /dollar /percent /ampersand
  /quoteright\n/parenleft /parenright /asterisk /plus /comma /hyphen /period
  /slash /zero /one\n/two /three /four /five /six /seven /eight /nine /colon
  /semicolon\n/less /equal /greater /question /at /A /B /C /D /E\n/F /G /H /I
  /J /K /L /M /N /O\n/P /Q /R /S /T /U /V /W /X /Y\n/Z /bracketleft
  /backslash /bracketright /asciicircum /underscore /quoteleft /a /b /c\n/d
  /e /f /g /h /i /j /k /l /m\n/n /o /p /q /r /s /t /u /v /w\n/x /y /z
  /braceleft /bar /braceright /asciitilde /.notdef /.notdef
  /.notdef\n/.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
  /.notdef /.notdef /.notdef\n/.notdef /.notdef /.notdef /.notdef /.notdef
  /.notdef /.notdef /.notdef /.notdef /.notdef\n/.notdef /.notdef /.notdef
  /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef\n/space
  /exclamdown /cent /sterling /currency /yen /brokenbar /section /dieresis
  /copyright\n/ordfeminine /guillemotleft /logicalnot /hyphen /registered
  /macron /degree /plusminus /twosuperior /threesuperior\n/acute /mu
  /paragraph /periodcentered /cedilla /onesuperior /ordmasculine
  /guillemotright /onequarter /onehalf\n/threequarters /questiondown /Agrave
  /Aacute /Acircumflex /Atilde /Adieresis /Aring /AE /Ccedilla\n/Egrave
  /Eacute /Ecircumflex /Edieresis /Igrave /Iacute /Icircumflex /Idieresis
  /Eth /Ntilde\n/Ograve /Oacute /Ocircumflex /Otilde /Odieresis /multiply
  /Oslash /Ugrave /Uacute /Ucircumflex\n/Udieresis /Yacute /Thorn /germandbls
  /agrave /aacute /acircumflex /atilde /adieresis /aring\n/ae /ccedilla
  /egrave /eacute /ecircumflex /edieresis /igrave /iacute /icircumflex
  /idieresis\n/eth /ntilde /ograve /oacute /ocircumflex /otilde /odieresis
  /divide /oslash /ugrave\n/uacute /ucircumflex /udieresis /yacute /thorn
  /ydieresis] /isolatin1encoding exch def\n/cp {closepath} bind def\n/c
  {curveto} bind def\n/f {fill} bind def\n/a {arc} bind def\n/ef {eofill}
  bind def\n/ex {exch} bind def\n/gr {grestore} bind def\n/gs {gsave} bind
  def\n/sa {save} bind def\n/rs {restore} bind def\n/l {lineto} bind def\n/m
  {moveto} bind def\n/rm {rmoveto} bind def\n/n {newpath} bind def\n/s
  {stroke} bind def\n/sh {show} bind def\n/slc {setlinecap} bind def\n/slj
  {setlinejoin} bind def\n/slw {setlinewidth} bind def\n/srgb {setrgbcolor}
  bind def\n/rot {rotate} bind def\n/sc {scale} bind def\n/sd {setdash} bind
  def\n/ff {findfont} bind def\n/sf {setfont} bind def\n/scf {scalefont} bind
  def\n/sw {stringwidth pop} bind def\n/tr {translate} bind
  def\n\n/ellipsedict 8 dict def\nellipsedict /mtrx matrix put\n/ellipse\n{
  ellipsedict begin\n \ \ /endangle exch def\n \ \ /startangle exch def\n
  \ \ /yrad exch def\n \ \ /xrad exch def\n \ \ /y exch def\n \ \ /x exch def
  \ \ /savematrix mtrx currentmatrix def\n \ \ x y tr xrad yrad sc\n \ \ 0 0
  1 startangle endangle arc\n \ \ savematrix setmatrix\n \ \ end\n}
  def\n\n/mergeprocs {\ndup length\n3 -1 roll\ndup\nlength\ndup\n5 1 roll\n3
  -1 roll\nadd\narray cvx\ndup\n3 -1 roll\n0 exch\nputinterval\ndup\n4 2
  roll\nputinterval\n} bind def\n/Times-Roman-latin1\n \ \ \ /Times-Roman
  findfont\n \ \ \ dup length dict begin\n\t{1 index /FID ne {def} {pop pop}
  ifelse} forall\n\t/Encoding isolatin1encoding def\n \ \ \ currentdict
  end\ndefinefont pop\n/Times-Italic-latin1\n \ \ \ /Times-Italic findfont\n
  \ \ \ dup length dict begin\n\t{1 index /FID ne {def} {pop pop} ifelse}
  forall\n\t/Encoding isolatin1encoding def\n \ \ \ currentdict
  end\ndefinefont pop\n/Times-Bold-latin1\n \ \ \ /Times-Bold findfont\n
  \ \ \ dup length dict begin\n\t{1 index /FID ne {def} {pop pop} ifelse}
  forall\n\t/Encoding isolatin1encoding def\n \ \ \ currentdict
  end\ndefinefont pop\n/Times-BoldItalic-latin1\n \ \ \ /Times-BoldItalic
  findfont\n \ \ \ dup length dict begin\n\t{1 index /FID ne {def} {pop pop}
  ifelse} forall\n\t/Encoding isolatin1encoding def\n \ \ \ currentdict
  end\ndefinefont pop\n/AvantGarde-Book-latin1\n \ \ \ /AvantGarde-Book
  findfont\n \ \ \ dup length dict begin\n\t{1 index /FID ne {def} {pop pop}
  ifelse} forall\n\t/Encoding isolatin1encoding def\n \ \ \ currentdict
  end\ndefinefont pop\n/AvantGarde-BookOblique-latin1\n
  \ \ \ /AvantGarde-BookOblique findfont\n \ \ \ dup length dict begin\n\t{1
  index /FID ne {def} {pop pop} ifelse} forall\n\t/Encoding isolatin1encoding
  def\n \ \ \ currentdict end\ndefinefont pop\n/AvantGarde-Demi-latin1\n
  \ \ \ /AvantGarde-Demi findfont\n \ \ \ dup length dict begin\n\t{1 index
  /FID ne {def} {pop pop} ifelse} forall\n\t/Encoding isolatin1encoding def\n
  \ \ \ currentdict end\ndefinefont pop\n/AvantGarde-DemiOblique-latin1\n
  \ \ \ /AvantGarde-DemiOblique findfont\n \ \ \ dup length dict begin\n\t{1
  index /FID ne {def} {pop pop} ifelse} forall\n\t/Encoding isolatin1encoding
  def\n \ \ \ currentdict end\ndefinefont pop\n/Bookman-Light-latin1\n
  \ \ \ /Bookman-Light findfont\n \ \ \ dup length dict begin\n\t{1 index
  /FID ne {def} {pop pop} ifelse} forall\n\t/Encoding isolatin1encoding def\n
  \ \ \ currentdict end\ndefinefont pop\n/Bookman-LightItalic-latin1\n
  \ \ \ /Bookman-LightItalic findfont\n \ \ \ dup length dict begin\n\t{1
  index /FID ne {def} {pop pop} ifelse} forall\n\t/Encoding isolatin1encoding
  def\n \ \ \ currentdict end\ndefinefont pop\n/Bookman-Demi-latin1\n
  \ \ \ /Bookman-Demi findfont\n \ \ \ dup length dict begin\n\t{1 index /FID
  ne {def} {pop pop} ifelse} forall\n\t/Encoding isolatin1encoding def\n
  \ \ \ currentdict end\ndefinefont pop\n/Bookman-DemiItalic-latin1\n
  \ \ \ /Bookman-DemiItalic findfont\n \ \ \ dup length dict begin\n\t{1
  index /FID ne {def} {pop pop} ifelse} forall\n\t/Encoding isolatin1encoding
  def\n \ \ \ currentdict end\ndefinefont pop\n/Courier-latin1\n
  \ \ \ /Courier findfont\n \ \ \ dup length dict begin\n\t{1 index /FID ne
  {def} {pop pop} ifelse} forall\n\t/Encoding isolatin1encoding def\n
  \ \ \ currentdict end\ndefinefont pop\n/Courier-Oblique-latin1\n
  \ \ \ /Courier-Oblique findfont\n \ \ \ dup length dict begin\n\t{1 index
  /FID ne {def} {pop pop} ifelse} forall\n\t/Encoding isolatin1encoding def\n
  \ \ \ currentdict end\ndefinefont pop\n/Courier-Bold-latin1\n
  \ \ \ /Courier-Bold findfont\n \ \ \ dup length dict begin\n\t{1 index /FID
  ne {def} {pop pop} ifelse} forall\n\t/Encoding isolatin1encoding def\n
  \ \ \ currentdict end\ndefinefont pop\n/Courier-BoldOblique-latin1\n
  \ \ \ /Courier-BoldOblique findfont\n \ \ \ dup length dict begin\n\t{1
  index /FID ne {def} {pop pop} ifelse} forall\n\t/Encoding isolatin1encoding
  def\n \ \ \ currentdict end\ndefinefont pop\n/Helvetica-latin1\n
  \ \ \ /Helvetica findfont\n \ \ \ dup length dict begin\n\t{1 index /FID ne
  {def} {pop pop} ifelse} forall\n\t/Encoding isolatin1encoding def\n
  \ \ \ currentdict end\ndefinefont pop\n/Helvetica-Oblique-latin1\n
  \ \ \ /Helvetica-Oblique findfont\n \ \ \ dup length dict begin\n\t{1 index
  /FID ne {def} {pop pop} ifelse} forall\n\t/Encoding isolatin1encoding def\n
  \ \ \ currentdict end\ndefinefont pop\n/Helvetica-Bold-latin1\n
  \ \ \ /Helvetica-Bold findfont\n \ \ \ dup length dict begin\n\t{1 index
  /FID ne {def} {pop pop} ifelse} forall\n\t/Encoding isolatin1encoding def\n
  \ \ \ currentdict end\ndefinefont pop\n/Helvetica-BoldOblique-latin1\n
  \ \ \ /Helvetica-BoldOblique findfont\n \ \ \ dup length dict begin\n\t{1
  index /FID ne {def} {pop pop} ifelse} forall\n\t/Encoding isolatin1encoding
  def\n \ \ \ currentdict end\ndefinefont pop\n/Helvetica-Narrow-latin1\n
  \ \ \ /Helvetica-Narrow findfont\n \ \ \ dup length dict begin\n\t{1 index
  /FID ne {def} {pop pop} ifelse} forall\n\t/Encoding isolatin1encoding def\n
  \ \ \ currentdict end\ndefinefont pop\n/Helvetica-Narrow-Oblique-latin1\n
  \ \ \ /Helvetica-Narrow-Oblique findfont\n \ \ \ dup length dict
  begin\n\t{1 index /FID ne {def} {pop pop} ifelse} forall\n\t/Encoding
  isolatin1encoding def\n \ \ \ currentdict end\ndefinefont
  pop\n/Helvetica-Narrow-Bold-latin1\n \ \ \ /Helvetica-Narrow-Bold
  findfont\n \ \ \ dup length dict begin\n\t{1 index /FID ne {def} {pop pop}
  ifelse} forall\n\t/Encoding isolatin1encoding def\n \ \ \ currentdict
  end\ndefinefont pop\n/Helvetica-Narrow-BoldOblique-latin1\n
  \ \ \ /Helvetica-Narrow-BoldOblique findfont\n \ \ \ dup length dict
  begin\n\t{1 index /FID ne {def} {pop pop} ifelse} forall\n\t/Encoding
  isolatin1encoding def\n \ \ \ currentdict end\ndefinefont
  pop\n/NewCenturySchoolbook-Roman-latin1\n \ \ \ /NewCenturySchoolbook-Roman
  findfont\n \ \ \ dup length dict begin\n\t{1 index /FID ne {def} {pop pop}
  ifelse} forall\n\t/Encoding isolatin1encoding def\n \ \ \ currentdict
  end\ndefinefont pop\n/NewCenturySchoolbook-Italic-latin1\n
  \ \ \ /NewCenturySchoolbook-Italic findfont\n \ \ \ dup length dict
  begin\n\t{1 index /FID ne {def} {pop pop} ifelse} forall\n\t/Encoding
  isolatin1encoding def\n \ \ \ currentdict end\ndefinefont
  pop\n/NewCenturySchoolbook-Bold-latin1\n \ \ \ /NewCenturySchoolbook-Bold
  findfont\n \ \ \ dup length dict begin\n\t{1 index /FID ne {def} {pop pop}
  ifelse} forall\n\t/Encoding isolatin1encoding def\n \ \ \ currentdict
  end\ndefinefont pop\n/NewCenturySchoolbook-BoldItalic-latin1\n
  \ \ \ /NewCenturySchoolbook-BoldItalic findfont\n \ \ \ dup length dict
  begin\n\t{1 index /FID ne {def} {pop pop} ifelse} forall\n\t/Encoding
  isolatin1encoding def\n \ \ \ currentdict end\ndefinefont
  pop\n/Palatino-Roman-latin1\n \ \ \ /Palatino-Roman findfont\n \ \ \ dup
  length dict begin\n\t{1 index /FID ne {def} {pop pop} ifelse}
  forall\n\t/Encoding isolatin1encoding def\n \ \ \ currentdict
  end\ndefinefont pop\n/Palatino-Italic-latin1\n \ \ \ /Palatino-Italic
  findfont\n \ \ \ dup length dict begin\n\t{1 index /FID ne {def} {pop pop}
  ifelse} forall\n\t/Encoding isolatin1encoding def\n \ \ \ currentdict
  end\ndefinefont pop\n/Palatino-Bold-latin1\n \ \ \ /Palatino-Bold
  findfont\n \ \ \ dup length dict begin\n\t{1 index /FID ne {def} {pop pop}
  ifelse} forall\n\t/Encoding isolatin1encoding def\n \ \ \ currentdict
  end\ndefinefont pop\n/Palatino-BoldItalic-latin1\n
  \ \ \ /Palatino-BoldItalic findfont\n \ \ \ dup length dict begin\n\t{1
  index /FID ne {def} {pop pop} ifelse} forall\n\t/Encoding isolatin1encoding
  def\n \ \ \ currentdict end\ndefinefont pop\n/Symbol-latin1\n \ \ \ /Symbol
  findfont\ndefinefont pop\n/ZapfChancery-MediumItalic-latin1\n
  \ \ \ /ZapfChancery-MediumItalic findfont\n \ \ \ dup length dict
  begin\n\t{1 index /FID ne {def} {pop pop} ifelse} forall\n\t/Encoding
  isolatin1encoding def\n \ \ \ currentdict end\ndefinefont
  pop\n/ZapfDingbats-latin1\n \ \ \ /ZapfDingbats findfont\n \ \ \ dup length
  dict begin\n\t{1 index /FID ne {def} {pop pop} ifelse} forall\n\t/Encoding
  isolatin1encoding def\n \ \ \ currentdict end\ndefinefont pop\n28.346000
  -28.346000 scale\n-12.879289 -9.070711 translate\n%%EndProlog\n\n\n0.100000
  slw\n[] 0 sd\n[] 0 sd\n0 slj\n0 slc\n0.919356 0.919356 0.919356 srgb\nn
  14.000000 5.000000 m 16.000000 5.000000 l 16.000000 7.000000 l 17.000000
  7.000000 l 15.000000 9.000000 l 13.000000 7.000000 l 14.000000 7.000000 l
  f\n0.000000 0.000000 0.000000 srgb\nn 14.000000 5.000000 m 16.000000
  5.000000 l 16.000000 7.000000 l 17.000000 7.000000 l 15.000000 9.000000 l
  13.000000 7.000000 l 14.000000 7.000000 l cp s\nshowpage\n>|eps>||||||>

  <\with|font size|0.84>
    <\code>
      parse_time (t : <with|color|brown|string>) : <with|color|brown|int>

      \ \ {

      \ \ \ \ <with|color|black|<em|def> <with|color|magenta|<with|color|black|r
      =>>> Regexp (<with|color|magenta|"([0-9]+):([0-9]+)">);

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
    <item><verbatim|<with|color|magenta|rxmatch>> <emdash> a special function
    that triggers plugin execution

    <item>plugin produces bindings for named groups and removes names from
    regular expression
  </itemize>

  <new_page>

  <section|SQL queries macro>

  <with|font size|0.84|<\code>
    <with|font size|0.84|>sql_loop (conn,\ 

    \ \ \ \ \ \ \ \ \ \ <with|color|magenta|"SELECT <with|color|red|salary>,
    LOWER (name) <with|color|red|lname>>

    \ \ \ \ \ \ \ \ \ \ \ \ \ <with|color|magenta|FROM employees >

    \ \ \ \ \ \ \ \ \ \ \ \ \ <with|color|magenta|WHERE salary \<gtr\>
    <with|mode|math|><with|mode|math|>$(<with|color|red|min_salary * 3>)">,

    \ \ \ \ \ \ \ \ \ \ print_string (lname + ":" +\ 

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ itoa (salary)))
  </code>>

  \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ <postscript|<tuple|<raw_data|%!PS-Adobe-2.0
  EPSF-2.0\n%%Title: arrow-small.dia\n%%Creator: Dia
  v0.92-pre5\n%%CreationDate: Tue Nov \ 4 23:54:30 2003\n%%For:
  malekith\n%%Orientation: Portrait\n%%Magnification: 1.0000\n%%BoundingBox:
  0 0 126 60\n%%BeginSetup\n%%EndSetup\n%%EndComments\n%%BeginProlog\n[
  /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
  /.notdef /.notdef\n/.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
  /.notdef /.notdef /.notdef /.notdef\n/.notdef /.notdef /.notdef /.notdef
  /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef\n/.notdef /.notdef
  /space /exclam /quotedbl /numbersign /dollar /percent /ampersand
  /quoteright\n/parenleft /parenright /asterisk /plus /comma /hyphen /period
  /slash /zero /one\n/two /three /four /five /six /seven /eight /nine /colon
  /semicolon\n/less /equal /greater /question /at /A /B /C /D /E\n/F /G /H /I
  /J /K /L /M /N /O\n/P /Q /R /S /T /U /V /W /X /Y\n/Z /bracketleft
  /backslash /bracketright /asciicircum /underscore /quoteleft /a /b /c\n/d
  /e /f /g /h /i /j /k /l /m\n/n /o /p /q /r /s /t /u /v /w\n/x /y /z
  /braceleft /bar /braceright /asciitilde /.notdef /.notdef
  /.notdef\n/.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
  /.notdef /.notdef /.notdef\n/.notdef /.notdef /.notdef /.notdef /.notdef
  /.notdef /.notdef /.notdef /.notdef /.notdef\n/.notdef /.notdef /.notdef
  /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef\n/space
  /exclamdown /cent /sterling /currency /yen /brokenbar /section /dieresis
  /copyright\n/ordfeminine /guillemotleft /logicalnot /hyphen /registered
  /macron /degree /plusminus /twosuperior /threesuperior\n/acute /mu
  /paragraph /periodcentered /cedilla /onesuperior /ordmasculine
  /guillemotright /onequarter /onehalf\n/threequarters /questiondown /Agrave
  /Aacute /Acircumflex /Atilde /Adieresis /Aring /AE /Ccedilla\n/Egrave
  /Eacute /Ecircumflex /Edieresis /Igrave /Iacute /Icircumflex /Idieresis
  /Eth /Ntilde\n/Ograve /Oacute /Ocircumflex /Otilde /Odieresis /multiply
  /Oslash /Ugrave /Uacute /Ucircumflex\n/Udieresis /Yacute /Thorn /germandbls
  /agrave /aacute /acircumflex /atilde /adieresis /aring\n/ae /ccedilla
  /egrave /eacute /ecircumflex /edieresis /igrave /iacute /icircumflex
  /idieresis\n/eth /ntilde /ograve /oacute /ocircumflex /otilde /odieresis
  /divide /oslash /ugrave\n/uacute /ucircumflex /udieresis /yacute /thorn
  /ydieresis] /isolatin1encoding exch def\n/cp {closepath} bind def\n/c
  {curveto} bind def\n/f {fill} bind def\n/a {arc} bind def\n/ef {eofill}
  bind def\n/ex {exch} bind def\n/gr {grestore} bind def\n/gs {gsave} bind
  def\n/sa {save} bind def\n/rs {restore} bind def\n/l {lineto} bind def\n/m
  {moveto} bind def\n/rm {rmoveto} bind def\n/n {newpath} bind def\n/s
  {stroke} bind def\n/sh {show} bind def\n/slc {setlinecap} bind def\n/slj
  {setlinejoin} bind def\n/slw {setlinewidth} bind def\n/srgb {setrgbcolor}
  bind def\n/rot {rotate} bind def\n/sc {scale} bind def\n/sd {setdash} bind
  def\n/ff {findfont} bind def\n/sf {setfont} bind def\n/scf {scalefont} bind
  def\n/sw {stringwidth pop} bind def\n/tr {translate} bind
  def\n\n/ellipsedict 8 dict def\nellipsedict /mtrx matrix put\n/ellipse\n{
  ellipsedict begin\n \ \ /endangle exch def\n \ \ /startangle exch def\n
  \ \ /yrad exch def\n \ \ /xrad exch def\n \ \ /y exch def\n \ \ /x exch def
  \ \ /savematrix mtrx currentmatrix def\n \ \ x y tr xrad yrad sc\n \ \ 0 0
  1 startangle endangle arc\n \ \ savematrix setmatrix\n \ \ end\n}
  def\n\n/mergeprocs {\ndup length\n3 -1 roll\ndup\nlength\ndup\n5 1 roll\n3
  -1 roll\nadd\narray cvx\ndup\n3 -1 roll\n0 exch\nputinterval\ndup\n4 2
  roll\nputinterval\n} bind def\n/dpi_x 300 def\n/dpi_y 300 def\n/conicto {\n
  \ \ \ /to_y exch def\n \ \ \ /to_x exch def\n \ \ \ /conic_cntrl_y exch
  def\n \ \ \ /conic_cntrl_x exch def\n \ \ \ currentpoint\n \ \ \ /p0_y exch
  def\n \ \ \ /p0_x exch def\n \ \ \ /p1_x p0_x conic_cntrl_x p0_x sub 2 3
  div mul add def\n \ \ \ /p1_y p0_y conic_cntrl_y p0_y sub 2 3 div mul add
  def\n \ \ \ /p2_x p1_x to_x p0_x sub 1 3 div mul add def\n \ \ \ /p2_y p1_y
  to_y p0_y sub 1 3 div mul add def\n \ \ \ p1_x p1_y p2_x p2_y to_x to_y
  curveto\n} bind def\n/start_ol { gsave 1.1 dpi_x div dup scale} bind
  def\n/end_ol { closepath fill grestore } bind def\n28.346000 -28.346000
  scale\n-14.788197 -11.055902 translate\n%%EndProlog\n\n\n0.100000 slw\n[] 0
  sd\n[] 0 sd\n0 slj\n0 slc\n0.917647 0.917647 0.917647 srgb\nn 16.000000
  9.000000 m 18.000000 9.000000 l 18.000000 10.000000 l 19.000000 10.000000 l
  17.000000 11.000000 l 15.000000 10.000000 l 16.000000 10.000000 l
  f\n0.000000 0.000000 0.000000 srgb\nn 16.000000 9.000000 m 18.000000
  9.000000 l 18.000000 10.000000 l 19.000000 10.000000 l 17.000000 11.000000
  l 15.000000 10.000000 l 16.000000 10.000000 l cp s\nshowpage\n>|eps>||||||>

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
    <verbatim|<with|color|red|salary>> and <verbatim|<with|color|red|lname>>
    variables
  </itemize>
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
    <associate|toc-20|<tuple|<uninit>|10>>
    <associate|toc-10|<tuple|<uninit>|5>>
    <associate|gly-1|<tuple|1|?>>
    <associate|toc-11|<tuple|<uninit>|6>>
    <associate|toc-21|<tuple|<uninit>|11>>
    <associate|toc-12|<tuple|<uninit>|7>>
    <associate|toc-13|<tuple|<uninit>|7>>
    <associate|toc-14|<tuple|<uninit>|8>>
    <associate|toc-15|<tuple|<uninit>|8>>
    <associate|toc-16|<tuple|<uninit>|8>>
    <associate|toc-17|<tuple|<uninit>|9>>
    <associate|toc-18|<tuple|<uninit>|9>>
    <associate|toc-19|<tuple|<uninit>|10>>
    <associate|toc-1|<tuple|<uninit>|2>>
    <associate|toc-2|<tuple|<uninit>|2>>
    <associate|toc-3|<tuple|<uninit>|3>>
    <associate|toc-4|<tuple|<uninit>|3>>
    <associate|toc-5|<tuple|<uninit>|3>>
    <associate|toc-6|<tuple|<uninit>|4>>
    <associate|toc-7|<tuple|<uninit>|4>>
    <associate|toc-8|<tuple|<uninit>|5>>
    <associate|toc-9|<tuple|<uninit>|5>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      Reasons -- why new language?<value|toc-dots><pageref|toc-1>

      <with|left margin|<quote|1.5fn>|<with|font size|<quote|1.41>|And the
      most important reason><value|toc-dots><pageref|toc-2>>

      Method -- how to do it?<value|toc-dots><pageref|toc-3>

      <with|left margin|<quote|3fn>|Combine<value|toc-dots><pageref|toc-4>>

      And the Result<value|toc-dots><pageref|toc-5>

      Formal semantics<value|toc-dots><pageref|toc-6>

      Assertions and tests<value|toc-dots><pageref|toc-7>

      Hello world<value|toc-dots><pageref|toc-8>

      <with|left margin|<quote|3fn>|<value|toc-dots><pageref|toc-9>>

      Factorial<value|toc-dots><pageref|toc-10>

      Variants<value|toc-dots><pageref|toc-11>

      Functional values<value|toc-dots><pageref|toc-12>

      Binary search tree insertion<value|toc-dots><pageref|toc-13>

      Safe, Turing-complete macros<value|toc-dots><pageref|toc-14>

      <with|left margin|<quote|3fn>|Combine<value|toc-dots><pageref|toc-15>>

      <with|left margin|<quote|3fn>|Example
      uses<value|toc-dots><pageref|toc-16>>

      Regular expression macro<value|toc-dots><pageref|toc-17>

      <with|left margin|<quote|3fn>|Remarks<value|toc-dots><pageref|toc-18>>

      SQL queries macro<value|toc-dots><pageref|toc-19>

      <with|left margin|<quote|3fn>|Remarks<value|toc-dots><pageref|toc-20>>
    </associate>
  </collection>
</auxiliary>