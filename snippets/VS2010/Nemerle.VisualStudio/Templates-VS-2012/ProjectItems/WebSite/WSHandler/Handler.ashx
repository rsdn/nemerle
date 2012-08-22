<%@ WebHandler Language="Nemerle" Class="$safeitemrootname$" %>

using System;
using System.Web;

public class $safeitemrootname$ : IHttpHandler 
{
  public ProcessRequest(context : HttpContext) : void 
  {
    context.Response.ContentType = "text/plain";
    context.Response.Write("Hello World");
  }

  public IsReusable : bool
  {
    get { false; }
  }
}
