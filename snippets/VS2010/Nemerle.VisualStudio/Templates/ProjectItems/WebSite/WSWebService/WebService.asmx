<%@ WebService Language="Nemerle" Class="$safeitemrootname$" %>

using System;
using System.Web;
using System.Web.Services;
using System.Web.Services.Protocols;

[WebService(Namespace = "http://tempuri.org/")]
[WebServiceBinding(ConformsTo = WsiProfiles.BasicProfile1_1)]
$if$ ($targetframeworkversion$ == 3.5)// To allow this Web Service to be called from script, using ASP.NET AJAX, uncomment the following line. 
// [System.Web.Script.Services.ScriptService]
$endif$public class $safeitemrootname$  : System.Web.Services.WebService 
{

  [WebMethod]
  public HelloWorld() : string 
  {
    "Hello World";
  }

}

