<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
  version="1.0">
  <!--
  <xsl:output 
    method="xml" 
    doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd" 
    doctype-public="-//W3C//DTD XHTML 1.0 Transitional//EN"/>
  -->
    
  <xsl:template match="submission">
    <tr>
      <td>
        <a href="/edit?id={id}">
          <xsl:value-of select="first_name" />
          <xsl:text> </xsl:text>
          <xsl:value-of select="last_name" />
        </a>
      </td>
      <td>
        <xsl:value-of select="email" />
      </td>
      <td>
        <xsl:value-of select="university" />
      </td>
    </tr>
  </xsl:template>
  
  <xsl:template match="submissions">
    <html>
      <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        <title>Lista zgłoszeń</title>
      </head>
      <body>
        <h1>Lista zgłoszeń</h1>
        <table>
          <tr>
            <th>Imię i nazwisko</th>
            <th>E-mail</th>
            <th>Afillacja</th>
          </tr>
          <xsl:apply-templates select="submission" />
        </table>
      </body>
    </html>
  </xsl:template>
  
</xsl:stylesheet>
