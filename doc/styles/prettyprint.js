var keywordsCS = "";
var keywordsNem = "";

var keywordArray = 
  new Array ("abstract","event","new","struct","as","explicit","null","switch",
             "base","extern","object","this","bool","false","operator","throw",
             "break","finally","out","true","byte","fixed","override","try","case",
             "float","params","typeof","catch","for","private","uint","char",
             "foreach","protected","ulong","checked","goto","public",
             "unchecked","class","if","readonly","unsafe","const","implicit",
             "ref","ushort","continue","in","return","using","decimal","int",
             "sbyte","virtual","default","interface","sealed","volatile",
             "delegate","internal","short","void","do","is",
             "sizeof","while","double","lock","stackalloc","else","long","static",
             "enum","namespace","string");
var keywordNemArray = 
  new Array ("abstract","event","new","struct","null","match",
             "base","extern","object","this","bool","false","throw",
             "finally","out","true","byte","override","try",
             "float","params","typeof","catch","for","private","uint","char",
             "foreach","protected","ulong","checked","public",
             "unchecked","class","if","mutable",
             "ref","ushort","def","in","fun","using","decimal","int",
             "sbyte","virtual","interface","sealed","delegate","internal","short",
             "void","do","while","double","lock","else","long","static",
             "enum","namespace","string","implements");


function paintColors ()
{
  	keywordsCS = "(";
  	for (n=0; n<keywordArray.length; n++)
  		keywordsCS += "\\b" + keywordArray[n] + "\\b|";
  	keywordsCS += "string)";	
        keywordsNem = "(";
	for (n=0; n<keywordNemArray.length; n++)
		keywordsNem += "\\b" + keywordNemArray[n] + "\\b|";
	keywordsNem += "string)";
	
	var elems = document.getElementsByTagName ("xmp");
	for (n=elems.length - 1; n>=0; n--)
	{
		if (elems[n].className == "code-csharp") {
			format (elems [n], formatCs);
		}
		else if (elems[n].className == "code-xml") {
			format (elems [n], formatXml);
		}
		else if (elems[n].className == "code-nemerle") {
       			format (elems [n], formatNem);
		}
	}
}

function format (node, func)
{
	text = node.innerHTML;

	div = document.createElement("div");
	var className = node.className;
	
	// remove trailing/leading lines
	while (text.charAt (0) == "\n" || text.charAt (0) == "\r" )
		text = text.substr (1);
	
	while (text.charAt (text.length) == "\n" || text.charAt (text.length) == "\r" )
		text = text.splice (0, -1);

	div.innerHTML = func (text);
	node.parentNode.replaceChild(div, node);
	div.className = className;
}

function formatCs (text)
{

	var re = / /g;
	text = text.replace (re, "&nbsp;");

	re = /<(.*?)>/g;
	text = text.replace (re, "<___span style='color:red'_!_$1___/span_!_>");

	re = /</g;
	text = text.replace (re, "&lt;");

	re = />/g;
	text = text.replace (re, "&gt;");

	re = /___/g;
	text = text.replace (re, "<");

	re = /_!_/g;
	text = text.replace (re, ">");

	// cant get this one to work in the good syntax
	re = new RegExp ("\"((((?!\").)|\\\")*?)\"","g");
	text = text.replace (re,"<span style='color:purple'>\"$1\"</span>");

	re = /\/\/(((.(?!\"\<\/span\>))|"(((?!").)*)"\<\/span\>)*)(\r|\n|\r\n)/g;
	text = text.replace (re,"<span style='color:green'>//$1</span><br/>");
	
	re = new RegExp (keywordsCS,"g");
	text = text.replace (re,"<span style='color:blue'>$1</span>");
	
	re = /\t/g;
	text = text.replace (re,"&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
	
	re = /\n/g;
	text = text.replace (re,"<br/>");
	
	div = document.createElement("div");
	div.innerHTML = text;
	
	spans = div.getElementsByTagName ("span")
	for (i = 0; i < spans.length; i++) {
		if (spans [i].parentNode.nodeName == "SPAN") {
			spans [i].style.color = "";
		}
	}
	
	return div.innerHTML;
}

function formatXml (text)
{	
	var re = / /g;
	text = text.replace (re, "&nbsp;");

	re = /\t/g;
	text = text.replace (re, "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
	
	re = /\<\s*(\/?)\s*(.*?)\s*(\/?)\s*\>/g;
	text = text.replace (re,"{blue:&lt;$1}{maroon:$2}{blue:$3&gt;}");
	
	re = /{(\w*):(.*?)}/g;
	text = text.replace (re,"<span style='color:$1'>$2</span>");

	re = /"(.*?)"/g;
	text = text.replace (re,"\"<span style='color:purple'>$1</span>\"");

	re = /\r\n|\r|\n/g;
	text = text.replace (re, "<br/>");
	
	return text;
}

function formatNem (text)
{
	var re = / /g;
	text = text.replace (re, "&nbsp;");

	re = /<(.*?)>/g;
	text = text.replace (re, "<___span style='color:red'_!_$1___/span_!_>");

	re = /</g;
	text = text.replace (re, "&lt;");

	re = />/g;
	text = text.replace (re, "&gt;");

	re = /___/g;
	text = text.replace (re, "<");

	re = /_!_/g;
	text = text.replace (re, ">");

	// cant get this one to work in the good syntax
	re = new RegExp ("\"((((?!\").)|\\\")*?)\"","g");
	text = text.replace (re,"<span style='color:purple'>\"$1\"</span>");

	re = /\/\/(((.(?!\"\<\/span\>))|"(((?!").)*)"\<\/span\>)*)(\r|\n|\r\n)/g;
	text = text.replace (re,"<span style='color:green'>//$1</span><br/>");
	
        re = new RegExp (keywordsNem,"g");
	text = text.replace (re,"<span style='color:blue'>$1</span>");
	
	re = /\t/g;
	text = text.replace (re,"&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
 	
	re = /\n/g;
	text = text.replace (re,"<br/>");
 	
	div = document.createElement("div");
	div.innerHTML = text;
	
	spans = div.getElementsByTagName ("span")
	for (i = 0; i < spans.length; i++) {
		if (spans [i].parentNode.nodeName == "SPAN") {
			spans [i].style.color = "";
		}
	}
	
	return div.innerHTML;
 
}
