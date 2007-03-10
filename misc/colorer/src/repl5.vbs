Const ForReading = 1
Const ForWriting = 2

Set objFSO = CreateObject("Scripting.FileSystemObject")
Set objFile = objFSO.OpenTextFile("temp.hrc", ForReading)

strText = objFile.ReadAll
objFile.Close

strNewText = Replace(strText, "<define", "<region")
strNewText = Replace(strNewText, "value=""", "parent=""")
strNewText = Replace(strNewText, "lowpriority='lowpriority'", "priority='low'")

strNewText = Replace(strNewText, "<?xml version=""1.0"" encoding=""Windows-1251""?>", "")
strNewText = Replace(strNewText, "<!DOCTYPE hrc SYSTEM ""../hrc.dtd"">", "")
strNewText = Replace(strNewText, "<?xml-stylesheet type=""text/xsl"" href=""../hrc.xsl""?>", "")
strNewText = Replace(strNewText, "<hrc>", "")
strNewText = Replace(strNewText, "</hrc>", "")
strNewText = Replace(strNewText, "subst=", "subst-scheme=")
strNewText = Replace(strNewText, "<include name='base/nem_base.hrc'/>", "")
strNewText = Replace(strNewText, "<include name='base/nem_paren.hrc'/>", "")
strNewText = Replace(strNewText, "<include name='base/nem_comments.hrc'/>", "")
strNewText = Replace(strNewText, "<include name='base/nem_pp.hrc'/>", "")
strNewText = Replace(strNewText, "<include name='base/nem_num.hrc'/>", "")
strNewText = Replace(strNewText, "<include name='base/nem_char.hrc'/>", "")
strNewText = Replace(strNewText, "<include name='base/nem_str.hrc'/>", "")
strNewText = Replace(strNewText, "<include name='base/nem_key.hrc'/>", "")
strNewText = Replace(strNewText, "", "")
strNewText = "<?xml version=""1.0"" encoding=""windows-1251""?>"+vbCrLf+"<!DOCTYPE hrc PUBLIC ""-//Cail Lomecb//DTD Colorer HRC take5//EN"""+vbCrLf+"""http://colorer.sf.net/2003/hrc.dtd"">"+vbCrLf+""+"<hrc version=""take5"" xmlns=""http://colorer.sf.net/2003/hrc"""+vbCrLf+"xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"""+vbCrLf+"xsi:schemaLocation=""http://colorer.sf.net/2003/hrc http://colorer.sf.net/2003/hrc.xsd"">"+vbCrLf+""+"<type name=""nem2"">"+vbCrLf+""+"<import type=""def""/>"+vbCrLf+""+strNewText + "</type>"+vbCrLf+"</hrc>"

Set objFile = objFSO.OpenTextFile("temp.hrc", ForWriting)
objFile.WriteLine strNewText
objFile.Close