<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

  <xsl:template match="root">
    <html xmlns="http://www.w3.org/1999/xhtml" lang="en">
      <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        <title>
	    <xsl:value-of select="name" />
	</title>
      </head>
      <body>
	<table cellpadding="3" rules="cols">
	    <tr>	
	      <td>  </td>	
	      <td> <b> NAME </b> </td>
	      <td> <b> LAST ACCESED </b> </td>
	      <td> <b> SIZE </b> </td>
	   </tr>
        	<xsl:apply-templates select="directory" />
	        <xsl:apply-templates select="file" />
        </table>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="directory">
   <tr>	
    <td> <img src="/images/folder.gif" /> </td>	
    <td> <xsl:value-of select="@name" /> </td>
    <td> <xsl:value-of select="@LastAccessTime" /> </td>
    <td> <xsl:value-of select="@size" /> </td>
   </tr>
  </xsl:template>

  <xsl:template match="file">
   <tr>	
    <td> <img src="/images/unknown.gif" /> </td>		
    <td> <xsl:value-of select="@name" /> </td>
    <td> <xsl:value-of select="@LastAccessTime" /> </td>
    <td> <xsl:value-of select="@size" /> </td>
   </tr>
  </xsl:template>


</xsl:stylesheet>