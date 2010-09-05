<%@ Application Language="Nemerle" %>

<script runat="server">

  Application_Start(_sender : object, _e : EventArgs) : void
  {
      // Code that runs on application startup

  }

  Application_End(_sender : object, _e : EventArgs) : void
  {
      //  Code that runs on application shutdown

  }

  Application_Error(_sender : object, _e : EventArgs) : void
  {
      // Code that runs when an unhandled error occurs

  }

  Session_Start(_sender : object, _e : EventArgs) : void
  {
      // Code that runs when a new session is started

  }

  Session_End(_sender : object, _e : EventArgs) : void
  {
      // Code that runs when a session ends.
      // Note: The Session_End event is raised only when the sessionstate mode
      // is set to InProc in the Web.config file. If session mode is set to StateServer
      // or SQLServer, the event is not raised.

  }
</script>