using System;

using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.TextManager.Interop;
using Nemerle.Compiler;
using System.Collections.Generic;
using System.IO;

namespace Nemerle.VisualStudio.LanguageService
{
	public partial class NemerleSource
	{
		private IVsTextBufferCoordinator _bufferCoordinator = null;
		private int _primaryFileIndex = -1;
		private static Dictionary<int, int> _primaryToSecondaryFilesMap = new Dictionary<int, int>();
		private static Dictionary<IVsTextLines, string> _secondaryStubFiles = new Dictionary<IVsTextLines, string>();
		
		// Добавляет в проект файл-пустышку для редактируемого буфера. 
		// Файл требуется для нормальной работы интеграции, а например Spark View Engine не создает реального файла для secondary source
		public static string GetStubFileForSecondaryBuffer(IVsTextLines buffer)
		{
			if (_secondaryStubFiles.ContainsKey(buffer))
				return _secondaryStubFiles[buffer];
			else
			{
				string path = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString()) + NemerleConstants.FileExtension;

				_secondaryStubFiles.Add(buffer, path);
				return path;
			}
		}

		public static void RemoveStubFileForSecondaryBuffer(IVsTextLines buffer)
		{
			if (_secondaryStubFiles.ContainsKey(buffer))
			{
				var path = _secondaryStubFiles[buffer];
				_secondaryStubFiles.Remove(buffer);

				if (File.Exists(path))
				{
					try
					{
						File.Delete(path);
					}
					catch { }
				}
			}
		}

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
	}
}
