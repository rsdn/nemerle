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
        <a href="{/submissions/@secret}/edit.xml?id={id}">
          <xsl:value-of select="first_name" />
          <xsl:text> </xsl:text>
          <xsl:value-of select="last_name" />
        </a>
      </td>
      <td>
        <xsl:value-of select="email" />
      </td>
      <td>
        <xsl:value-of select="acc_name" />
        <xsl:text> </xsl:text>
        <xsl:value-of select="acc_address" />
        <xsl:text> </xsl:text>
        <xsl:value-of select="acc_nip" />
      </td>
      <td>
        <xsl:value-of select="university" />
      </td>
      <td>
        <xsl:if test="normalize-space(vega)">wegetarianiec, </xsl:if>
        <xsl:if test="normalize-space(single_room)">1-osobowiec, </xsl:if>
        <xsl:if test="normalize-space(treking)">czwartkowiec, </xsl:if>
        <xsl:if test="normalize-space(ref_title)">referat, </xsl:if>
        <xsl:if test="normalize-space(bus)">autokar, </xsl:if>
      </td>
      <td>
        <xsl:value-of select="remarks" />
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
        <table border="1">
          <tr>
            <th>Imię i nazwisko</th>
            <th>E-mail</th>
            <th>Konto</th>
            <th>Afillacja</th>
            <th>Flagi</th>
            <th>Uwagi</th>
          </tr>
          <xsl:apply-templates select="submission" />
        </table>
      </body>
    </html>
  </xsl:template>
  
</xsl:stylesheet>
