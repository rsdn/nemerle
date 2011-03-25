using System;
using System.ComponentModel.Design;

namespace Nemerle.VisualStudio.Project
{
	/// <summary>
	/// CommandIDs matching the commands defined items from PkgCmdID.h and guids.h
	/// </summary>
	public sealed class MenuCmd
	{
		internal static readonly Guid guidNemerleProjectCmdSet = new Guid(NemerleConstants.ProjectCmdSetGuidString);		

		internal enum CmdId
		{
			ESC = 103,

			// Menus

			RefactoringTopMenu   = 0x0100,
			SmartTagContextMenu  = 0x0101,

			// Comands IDs

			Rename                   = 0x2003,
			Inline                   = 0x2004,
			ExtendSelection          = 0x2005,
			ShrinkSelection          = 0x2006,
			GoToFile                 = 0x2007,
			ImplementInterface       = 0x2008,
			OverrideMembers          = 0x2009,

			SetAsMain = 0x3001,


			FindInheritors = 0x3103,

			Options = 0x3106,

			AstToolWindow = 0x3107,

			AddHighlighting = 0x3108,
			RemoveLastHighlighting = 0x3109,

			FindInheritorsCtxt = 0x3110,

			GoToType = 0x3112,

			SourceOutlinerWindow = 0x3113,

			IplementInterface = 0x3114
		};

		static CommandID MakeCmd(CmdId id) { return new CommandID(guidNemerleProjectCmdSet, (int)id); }

		internal static readonly CommandID SetAsMain = MakeCmd(CmdId.SetAsMain);

		internal static readonly CommandID ExtendSelection = MakeCmd(CmdId.ExtendSelection);
		internal static readonly CommandID ShrinkSelection = MakeCmd(CmdId.ShrinkSelection);

		internal static readonly CommandID FindInheritors = MakeCmd(CmdId.FindInheritors);

		internal static readonly CommandID Rename = MakeCmd(CmdId.Rename);
		internal static readonly CommandID Inline = MakeCmd(CmdId.Inline);

		internal static readonly CommandID Options = MakeCmd(CmdId.Options);

		internal static readonly CommandID AstToolWindow = MakeCmd(CmdId.AstToolWindow);

		internal static readonly CommandID AddHighlighting = MakeCmd(CmdId.AddHighlighting);
		internal static readonly CommandID RemoveLastHighlighting = MakeCmd(CmdId.RemoveLastHighlighting);

		internal static readonly CommandID FindInheritorsCtxt = MakeCmd(CmdId.FindInheritorsCtxt);

		internal static readonly CommandID GoToFile = MakeCmd(CmdId.GoToFile);
		internal static readonly CommandID GoToType = MakeCmd(CmdId.GoToType);

		internal static readonly CommandID SourceOutlinerWindow = MakeCmd(CmdId.SourceOutlinerWindow);
	}
}

