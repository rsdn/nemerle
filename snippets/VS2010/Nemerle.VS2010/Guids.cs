// Guids.cs
// MUST match guids.h
using System;

namespace Nemerle.VS2010
{
    static class GuidList
    {
        public const string guidNemerle_VS2010PkgString =
            "96bf4c26-d94e-43bf-a56a-f8500b52bca1";
        public const string guidNemerle_VS2010ProjectCmdSetString =
            "72c23e1d-f389-410a-b5f1-c938303f1391";
        public const string guidNemerle_VS2010ProjectFactoryString =
            "471EC4BB-E47E-4229-A789-D1F5F83B52D4";

        public static readonly Guid guidNemerle_VS2010ProjectCmdSet =
            new Guid(guidNemerle_VS2010ProjectCmdSetString);
        public static readonly Guid guidNemerle_VS2010ProjectFactory =
            new Guid(guidNemerle_VS2010ProjectFactoryString);
    };

}