Const ForReading = 1
Const ForWriting = 2

Set objFSO = CreateObject("Scripting.FileSystemObject")
Set objFile = objFSO.OpenTextFile("temp.hrc", ForReading)

strText = objFile.ReadAll
objFile.Close

strNewText = strText
strNewText = Replace(strNewText, "<?xml version=""1.0"" encoding=""Windows-1251""?>", "")
strNewText = Replace(strNewText, "<!DOCTYPE hrc SYSTEM ""../hrc.dtd"">", "")
strNewText = Replace(strNewText, "<?xml-stylesheet type=""text/xsl"" href=""../hrc.xsl""?>", "")
strNewText = Replace(strNewText, "<hrc>", "")
strNewText = Replace(strNewText, "</hrc>", "")
strNewText = Replace(strNewText, "<include name='base/nem_base.hrc'/>", "")
strNewText = Replace(strNewText, "<include name='base/nem_paren.hrc'/>", "")
strNewText = Replace(strNewText, "<include name='base/nem_comments.hrc'/>", "")
strNewText = Replace(strNewText, "<include name='base/nem_pp.hrc'/>", "")
strNewText = Replace(strNewText, "<include name='base/nem_num.hrc'/>", "")
strNewText = Replace(strNewText, "<include name='base/nem_char.hrc'/>", "")
strNewText = Replace(strNewText, "<include name='base/nem_str.hrc'/>", "")
strNewText = Replace(strNewText, "<include name='base/nem_key.hrc'/>", "")
strNewText = Replace(strNewText, "", "")
strNewText = "<?xml version=""1.0"" encoding=""Windows-1251""?>"+vbCrLf+"<!DOCTYPE hrc SYSTEM ""../hrc.dtd"">"+vbCrLf+"<?xml-stylesheet type=""text/xsl"" href=""../hrc.xsl""?>"+vbCrLf+"<hrc>" + vbCrLf + strNewText + vbCrLf+"</hrc>"

Set objFile = objFSO.OpenTextFile("temp.hrc", ForWriting, true)
objFile.WriteLine strNewText
objFile.Close