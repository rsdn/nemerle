using System;

using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.TextManager.Interop;
using Nemerle.Compiler;
using System.Collections.Generic;

namespace Nemerle.VisualStudio.LanguageService
{
	public partial class NemerleSource
	{
		private IVsTextBufferCoordinator _bufferCoordinator = null;
		private int _primaryFileIndex = -1;
		private static Dictionary<int, int> _primaryToSecondaryFilesMap = new Dictionary<int, int>();
		
		public static int GetSecondaryFileIndex(int primaryFileIndex)
		{
			if (HasSecondarySource(primaryFileIndex))
				return _primaryToSecondaryFilesMap[primaryFileIndex];
			else
				return -1;
		}

		public static bool HasSecondarySource(int fileIndex)
		{
			return _primaryToSecondaryFilesMap.ContainsKey(fileIndex);
		}

		public bool IsSecondarySource
		{
			get { return _bufferCoordinator != null; }
		}

		internal void SetBufferCoordinator(IVsTextBufferCoordinator coordinator)
		{
			_bufferCoordinator = coordinator;

			var primaryBuffer = GetPrimaryTextLines();
			_primaryFileIndex = Location.GetFileIndex(FilePathUtilities.GetFilePath(primaryBuffer));
			_primaryToSecondaryFilesMap[_primaryFileIndex] = FileIndex;
		}

		public Location MapSecondaryToPrimaryLocation(Location secondaryLocation)
		{
			TextSpan[] span = new TextSpan[1];
			_bufferCoordinator.MapSecondaryToPrimarySpan(Utils.SpanFromLocation(secondaryLocation), span);

			return Utils.LocationFromSpan(_primaryFileIndex, span[0]);
		}

		public IVsTextLines GetPrimaryTextLines()
		{
			if (_bufferCoordinator == null)
				return null;

			IVsTextLines buffer = null;

			_bufferCoordinator.GetPrimaryBuffer(out buffer);

			return buffer;
		}

		public override CompletionSet CreateCompletionSet()
		{
			return new NemerleCompletionSet(LanguageService.GetImageList(), this);
		}
	}
}
