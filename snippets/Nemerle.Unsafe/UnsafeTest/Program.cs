using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Runtime.InteropServices;
using System.Reflection.Emit;

using Type = System.Char;
//using Type = System.String;

namespace UnsafeTest
{
  unsafe class Program
  {
    static int[] ary = new int[] { 1, 2, 3 };
    //static int[] ary = new int[0];

    static void Main(string[] args)
    {
      var x = 1;
      //typeof(int).MakePointerType()
      //Console.WriteLine(ary[x]);

      //typeof(int).MakePointerType()


      //var handle = GCHandle.Alloc(ary, GCHandleType.Pinned);
      //int* ptr = (int*)handle.AddrOfPinnedObject();

      //System.Diagnostics.Trace.Assert(false);
      //var ptr = ary;
      fixed (int* ptr = &ary[1])
      {
        int* ptr2 = ptr;
        ++ptr2;
        Console.WriteLine(ptr2[x]);

        //handle.Free();
        //handle = GCHandle.Alloc(new int[] { 42, 43, 44 }, GCHandleType.Pinned);
        //ptr = (int*)handle.AddrOfPinnedObject();
        //Console.WriteLine(ptr[x]);
      }


      //var str = "abcde";

      //fixed (Type* ptr = str)
      //{
      //  Console.WriteLine(ptr[x]);
      //}
    }
  }
}
