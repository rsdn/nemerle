<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version='1.0'>
<xsl:import href="/home/users/malekith/db2latex-0.7/xsl/docbook.xsl"/>
<xsl:output method="text" encoding="ISO-8859-1" indent="yes"/>
<xsl:variable name="latex.override">
\documentclass[english,a4paper]{article}
\usepackage[dvips]{hyperref}
\usepackage{fancyvrb}
</xsl:variable>
</xsl:stylesheet>
