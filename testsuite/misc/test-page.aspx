<%@ language=Nemerle %>

  <script language="Nemerle" runat="server">
    variant Trr {
        | Node { x : Trr; y : Trr; }
        | Leaf { b : string; }
    }

    Page_Load(_ : object, _ : EventArgs) : void {
      Message.Text = "You last time accessed this page at: " + DateTime.Now.ToString ();
    }

    EnterBtn_Click(_ : object, _ : EventArgs) : void {
      Message.Text = "Hi " + Name.Text + ", welcome to ASP.NET!";
    }
  </script>

<html>
      <form action="controls3.aspx" runat=server>

          <font face="Verdana"> 

             Please enter your name: <asp:textbox id="Name" runat=server/> 
                                     <asp:button text="Enter" Onclick="EnterBtn_Click" runat=server/>

             <p>

             <% 
                def t = Node (Node (Leaf ("a"), Leaf ("b")), Node (Leaf ("c"), Node (Leaf ("d"), Leaf ("e"))));
                def print_it (n) {
                  | Leaf (str) => Response.Write ($"<li>$str</li>")
                  | Node (n1, n2) => 
                    Response.Write ("<li><ul>"); 
                    print_it (n1); print_it (n2); 
                    Response.Write ("</ul></li>")
                }
                Response.Write ("<ul>");
                print_it (t);
                Response.Write ("</ul>");
             %>

             <asp:label id="Message"  runat=server/>

          </font>

       </form>

</html>
