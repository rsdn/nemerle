using System;
using System.Runtime.InteropServices;

namespace MyExtrn
{     
 public class Offsets
 {
  [DllImport("./getoffsets.so")]
  static extern string GetOffsets(string dev);	
    
  public static string GetOffsetString(string dev) {
   return GetOffsets(dev);
  }
 }
}

