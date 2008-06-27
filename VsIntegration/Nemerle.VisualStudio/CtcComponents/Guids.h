/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
This code is licensed under the Visual Studio SDK license terms.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.

***************************************************************************/

//
// This file is used to define the Guids used to identify the command groups
// created by the sample.
//


// Guid of this package
// FBA89739-12DE-4598-9374-DF0082489563
#define guidNemerleProjectPkg {0xfba89739, 0x12de, 0x4598, {0x93, 0x74, 0xdf, 0x00, 0x82, 0x48, 0x95, 0x63} }
//phantom: don't know, it's needed or not
#ifdef DEFINE_GUID
DEFINE_GUID(CLSID_guidNemerleProjectCmdSet, 
0xfba89739, 0x12de, 0x4598, 0x93, 0x74, 0xdf, 0x00, 0x82, 0x48, 0x95, 0x63);
#endif

// Guid of the command set containing the command IDs of this package
// {D6DDF8E8-9A9E-425c-AB18-7BBCC70A6489}
#define guidNemerleProjectCmdSet {0xd6ddf8e8, 0x9a9e, 0x425c, {0xab, 0x18, 0x7b, 0xbc, 0xc7, 0x0a, 0x64, 0x89} }
//phantom: don't know,  if it's needed or not
#ifdef DEFINE_GUID
DEFINE_GUID(CLSID_guidNemerleProjectCmdSet, 
0xd6ddf8e8, 0x9a9e, 0x425c, 0xab, 0x18, 0x7b, 0xbc, 0xc7, 0x0a, 0x64, 0x89);
#endif

// Guid of the command set containing the command IDs of this package
// {EDCC3B7F-0BAD-11DB-BC1A-00112FDE8B61}
#define guidNemerleEditorFactory {0xEDCC3B7F, 0x0BAD, 0x11DB, {0xBC, 0x1A, 0x00, 0x11, 0x2F, 0xDE, 0x8B, 0x61} }
//phantom: don't know, it's needed or not
#ifdef DEFINE_GUID
DEFINE_GUID(CLSID_guidNemerleEditorFactory, 
0xEDCC3B7F, 0x0BAD, 0x11DB, 0xBC, 0x1A, 0x00, 0x11, 0x2F, 0xDE, 0x8B, 0x61);
#endif
