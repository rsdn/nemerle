<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

  <xsl:template match="/">
    <html xmlns="http://www.w3.org/1999/xhtml" lang="pl">
      <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        <title>XVIII Forum Informatyki Teoretycznej - Zgłoszenia</title>
        <link rel="stylesheet" href="/fit/style.css" />
      </head>
      <body>
        <table class="outer-menu">
          <tbody>
            <tr>
              <td>
                <table class="menu">
                  <tbody>
                    <tr>
                      <td align="left">
                        <a href="index.xml">Strona główna</a> :: 
                        <a href="register.xml">Rejestracja</a>
                      </td>
                      <td align="right">
                        <a href="#top-of-page">Top</a> :: <a href="#bottom-of-page">Bottom</a>
                      </td>
                    </tr>
                  </tbody>
                </table>
              </td>
            </tr>
          </tbody>
        </table>
        <a name="top-of-page" id="top-of-page"></a>
        <h1>XVIII Forum Informatyki Teoretycznej</h1>

        <table width="100%" border="0" align="left" cellspacing="0" cellpadding="0">
          <tbody>
            <tr>
              <td width="80%">
                <xsl:apply-templates select="page" />
                <br />
              </td>
              <td width="*"><![CDATA[ ]]></td>
            </tr>
            <tr>
              <td colspan="2" width="100%">
                <hr width="90%" size="1" noshade="noshade" />
                <div class="copyright">
                  Powered by <a href="http://www.nemerle.org">Nemerle</a>.
                  <br />
                  Copyright © 2004 University of Wrocław
                  <a name="bottom-of-page" id="bottom-of-page"></a>
                </div>
              </td>
            </tr>
          </tbody>
        </table>
      </body>
    </html>
  </xsl:template>
  
  <xsl:template match="page">
    <h2><xsl:value-of select="@subtitle" /></h2>
    <xsl:apply-templates select="page-header" />
    <xsl:apply-templates select="section" />
  </xsl:template>

  <xsl:template match="page-header">
    <xsl:apply-templates select="*" />
    <br />
    <hr width="90%" size="1" noshade="noshade" />
  </xsl:template>
    
  <xsl:template match="section">
    <xsl:call-template name="anchor">
      <xsl:with-param name="anchor-name">
        <xsl:value-of select="@anchor" />
      </xsl:with-param>
    </xsl:call-template>
    <h4><xsl:value-of select="@title" /></h4>
    <xsl:apply-templates select="*" />
    <br />
  </xsl:template>

  <xsl:template match="p">
    <p>
      <xsl:apply-templates />
    </p>  
  </xsl:template>

  <xsl:template match="table">
    <table class="realTable" border="0" cellspacing="0">
      <xsl:apply-templates />  
    </table>
  </xsl:template>

  <xsl:template match="tr">
    <tr>
      <xsl:apply-templates />  
    </tr>
  </xsl:template>

  <xsl:template match="td">
    <td>
      <xsl:apply-templates />  
    </td>
  </xsl:template>

  <xsl:template match="form">
    <form id="{@id}" action="{@action}" method="{@method}">
      <xsl:apply-templates />  
    </form>
  </xsl:template>

  <xsl:template match="input">
    <input type="{@type}">
      <xsl:if test="@checked != ''">
        <xsl:attribute name="checked">checked</xsl:attribute>
      </xsl:if> 
      <xsl:if test="@name != ''">
        <xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
      </xsl:if> 
      <xsl:if test="@value != ''">
        <xsl:attribute name="value"><xsl:value-of select="@value"/></xsl:attribute>
      </xsl:if> 
      <xsl:apply-templates />  
    </input>
  </xsl:template>

  <xsl:template match="textarea">
    <textarea name="{@name}" cols="{@cols}" rows="{@rows}">
      <xsl:apply-templates />  
    </textarea>
  </xsl:template>

  <xsl:template match="br">
    <br />
  </xsl:template>

  <xsl:template match="ul">
    <xsl:copy-of select="*" />
  </xsl:template>
  
  <xsl:template match="enumerate">
    <table align="center" width="80%" border="0" cellspacing="0" class="code">
      <xsl:apply-templates select="enum" />
    </table>  
  </xsl:template>

  <xsl:template match="enum">
    <tr>
      <td width="40%" class="realtable"><xsl:value-of select="@title" /></td>
      <td width="*" class="realtable"><xsl:apply-templates /></td>
    </tr>
  </xsl:template>
  
  <xsl:template match="pre">
    <pre>
      <xsl:apply-templates />
    </pre>
  </xsl:template>

  <xsl:template match="a">
    <a href="{@href}"><xsl:value-of select="."/></a>
  </xsl:template>


  <!-- HELPER PROCEDURES -->
  
  <xsl:template name="anchor">
    <xsl:param name="anchor-name">default</xsl:param>
    <a name="{$anchor-name}" id="{$anchor-name}" />
  </xsl:template>

</xsl:stylesheet>
