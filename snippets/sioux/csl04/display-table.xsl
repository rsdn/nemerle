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
        <xsl:value-of select="title" />
        <xsl:text> </xsl:text>
        <xsl:value-of select="organization" />
        <xsl:text> </xsl:text>
        <xsl:value-of select="address" />
        <xsl:text> </xsl:text>
        <xsl:value-of select="zip_city" />
        <xsl:text> </xsl:text>
        <xsl:value-of select="country" />
        <xsl:text> </xsl:text>
        <xsl:value-of select="fax" />
        <xsl:text> </xsl:text>
        <xsl:value-of select="fee_roommate" />
        <xsl:text> </xsl:text>
        <xsl:value-of select="accompany_name" />
        <xsl:text> </xsl:text>
        <xsl:value-of select="arrive_airport_time" />
        <xsl:text> </xsl:text>
        <xsl:value-of select="arrive_center_time" />
        <xsl:text> </xsl:text>
        <xsl:value-of select="depart_friday_time" />
        <xsl:text> </xsl:text>
        <xsl:value-of select="depart_saturday_time" />
        <xsl:text> </xsl:text>
      </td>
      <td>
        <xsl:if test="normalize-space(fee_regular)">pełnopłatny, </xsl:if>
        <xsl:if test="normalize-space(fee_student)">studencina, </xsl:if>
        <xsl:if test="normalize-space(vega)">wegetarianiec, </xsl:if>
        <xsl:if test="normalize-space(accompany)">z towarzystwem, </xsl:if>
        <xsl:if test="normalize-space(accompany_vega)">towarz. wegetariańskie, </xsl:if>
        <xsl:if test="normalize-space(arrive_center)">przyjazd z centrum, </xsl:if>
        <xsl:if test="normalize-space(arrive_directly)">przyjazd Karpacz, </xsl:if>
        <xsl:if test="normalize-space(arrive_book)">przyjazd z hotlem, </xsl:if>
        <xsl:if test="normalize-space(arrive_airport)">przyjazd z lotniska, </xsl:if>
        <xsl:if test="normalize-space(depart_book)">hotel wyjazd, </xsl:if>
        <xsl:if test="normalize-space(depart_friday)">wyjazd piątek, </xsl:if>
        <xsl:if test="normalize-space(depart_stay)">wyjazd później, </xsl:if>
        <xsl:if test="normalize-space(depart_saturday)">wyjazd sobota, </xsl:if>
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
            <th>Dane</th>
            <th>Flagi</th>
            <th>Uwagi</th>
          </tr>
          <xsl:apply-templates select="submission" />
        </table>
      </body>
    </html>
  </xsl:template>
  
</xsl:stylesheet>
