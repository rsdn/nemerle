<%@ Page Language="Nemerle" %>

<script runat="server">

    protected override OnLoad(e : EventArgs) : void {
        base.OnLoad(e);

        def originalPath = Request.Path;
        HttpContext.Current.RewritePath(Request.ApplicationPath, false);

        def httpHandler : IHttpHandler = MvcHttpHandler();
        httpHandler.ProcessRequest(HttpContext.Current);

        HttpContext.Current.RewritePath(originalPath, false);
    }

</script>
