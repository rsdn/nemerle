(*
 * Copyright (c) 2010  David Sorokin <david.sorokin@gmail.com>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *    1. Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *    2. Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
 * NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

namespace Nemerle.ComputationExpressions

open Nemerle.Builtins

module FSharpAsync =

    type FSharpAsyncBuilder () =
    
        member x.Bind (m, k: Function<'a, Async<'b>>) =
            async.Bind (m, fun a -> k.apply (a))
            
        member x.Bind (m: Async<unit>, k: Function<Async<'a>>) =
            async.Bind (m, fun _ -> k.apply ())
            
        member x.Zero () = 
            async.Zero ()
        
        member x.Delay (k : Function<Async<'a>>) = 
            async.Delay (fun () -> k.apply ())
            
        member x.Return (a) =
            async.Return (a)
            
        member x.ReturnComp (m) =
            async.ReturnFrom (m)
            
        member x.Using (g, k: Function<'a, Async<'b>>) =
            async.Using (g, fun a -> k.apply (a))
            
        member x.While (p: Function<bool>, m: Async<unit>) =
            async.While ((fun () -> p.apply ()), m)
            
        member x.ForEach (coll, k: Function<'a, Async<unit>>) =
            async.For (coll, (fun a -> k.apply (a)))
            
        member x.Combine (m1: Async<unit>, m2) =
            async.Combine (m1, m2)
            
        member x.TryFinally (m, h: FunctionVoid) =
            async.TryFinally (m, fun () -> h.apply_void ())
            
        member x.TryCatch (m, k: Function<exn, Async<'a>>) =
            async.TryWith (m, fun e -> k.apply (e))        

    let fsasync = new FSharpAsyncBuilder ()
