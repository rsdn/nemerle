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
        <xsl:if test="normalize-space(fee_regular)">full paid, </xsl:if>
        <xsl:if test="normalize-space(fee_student)">student, </xsl:if>
        <xsl:if test="normalize-space(vega)">vagetarian, </xsl:if>
        <xsl:if test="normalize-space(accompany)">with company, </xsl:if>
        <xsl:if test="normalize-space(accompany_vega)">vegetarian company, </xsl:if>
        <xsl:if test="normalize-space(arrive_center)">arrive center, </xsl:if>
        <xsl:if test="normalize-space(arrive_directly)">arrive Karpacz, </xsl:if>
        <xsl:if test="normalize-space(arrive_book)">arrive with hotel, </xsl:if>
        <xsl:if test="normalize-space(arrive_airport)">arrive to airport, </xsl:if>
        <xsl:if test="normalize-space(depart_book)">departure hotel, </xsl:if>
        <xsl:if test="normalize-space(depart_friday)">departure Friday, </xsl:if>
        <xsl:if test="normalize-space(depart_stay)">departure later, </xsl:if>
        <xsl:if test="normalize-space(depart_saturday)">departute saturday, </xsl:if>
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
        <title>Submission list</title>
      </head>
      <body>
        <h1>Submission list</h1>
        <table border="1">
          <tr>
            <th>Name</th>
            <th>E-mail</th>
            <th>Data</th>
            <th>Flags</th>
            <th>Remarks</th>
          </tr>
          <xsl:apply-templates select="submission" />
        </table>
      </body>
    </html>
  </xsl:template>
  
</xsl:stylesheet>
