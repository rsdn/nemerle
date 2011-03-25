using System;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.InteropServices;

using Microsoft.VisualStudio;
using Microsoft.VisualStudio.TextManager.Interop;

namespace Nemerle.VisualStudio.LanguageService
{
	internal class SnippetsEnumerator : IEnumerable<VsExpansion>
	{
		/// <summary>
		/// This structure is used to facilitate the interop calls to the method
		/// exposed by IVsExpansionEnumeration.
		/// </summary>
		[StructLayout(LayoutKind.Sequential)]
		struct ExpansionBuffer
		{
			public IntPtr _pathPtr;
			public IntPtr _titlePtr;
			public IntPtr _shortcutPtr;
			public IntPtr _descriptionPtr;
		}

		IVsTextManager2 _textManager;
		Guid			_languageGuid;
		bool			_shortcutOnly;

		public SnippetsEnumerator(IVsTextManager2 textManager, Guid languageGuid)
		{
			if (null == textManager)
				throw new ArgumentNullException("textManager");

			_textManager  = textManager;
			_languageGuid = languageGuid;
		}

		public bool ShortcutOnly
		{
			get { return _shortcutOnly;  }
			set { _shortcutOnly = value; }
		}

		#region IEnumerable<VsExpansion> Members

		public IEnumerator<VsExpansion> GetEnumerator()
		{
			IVsExpansionManager expansionManager;

			ErrorHandler.ThrowOnFailure(_textManager.GetExpansionManager(out expansionManager));

			IVsExpansionEnumeration enumerator;
			int					 onlyShortcut = ShortcutOnly ? 1 : 0;

			ErrorHandler.ThrowOnFailure(
				expansionManager.EnumerateExpansions(_languageGuid, onlyShortcut, null, 0, 0, 0, out enumerator));

			ExpansionBuffer buffer = new ExpansionBuffer();
			GCHandle		handle = GCHandle.Alloc(buffer, GCHandleType.Pinned);

			try
			{
				uint fetched;
				int  hr;

				while ((hr = enumerator.Next(1, new IntPtr[] { handle.AddrOfPinnedObject() }, out fetched)) == VSConstants.S_OK)
				{
					buffer = (ExpansionBuffer)handle.Target;

					try
					{
						handle.Free();

						if (IntPtr.Zero != buffer._shortcutPtr)
						{
							VsExpansion expansion = new VsExpansion();

							expansion.shortcut = Marshal.PtrToStringBSTR(buffer._shortcutPtr);

							if (IntPtr.Zero != buffer._descriptionPtr)
								expansion.description = Marshal.PtrToStringBSTR(buffer._descriptionPtr);

							if (IntPtr.Zero != buffer._pathPtr)
								expansion.path = Marshal.PtrToStringBSTR(buffer._pathPtr);

							if (IntPtr.Zero != buffer._titlePtr)
								expansion.title = Marshal.PtrToStringBSTR(buffer._titlePtr);

							yield return expansion;

							handle = GCHandle.Alloc(buffer, GCHandleType.Pinned);
						}
					}
					finally
					{
						if (IntPtr.Zero != buffer._descriptionPtr)
						{
							Marshal.FreeBSTR(buffer._descriptionPtr);
							buffer._descriptionPtr = IntPtr.Zero;
						}

						if (IntPtr.Zero != buffer._pathPtr)
						{
							Marshal.FreeBSTR(buffer._pathPtr);
							buffer._pathPtr = IntPtr.Zero;
						}

						if (IntPtr.Zero != buffer._shortcutPtr)
						{
							Marshal.FreeBSTR(buffer._shortcutPtr);
							buffer._shortcutPtr = IntPtr.Zero;
						}

						if (IntPtr.Zero != buffer._titlePtr)
						{
							Marshal.FreeBSTR(buffer._titlePtr);
							buffer._titlePtr = IntPtr.Zero;
						}
					}
				}

				ErrorHandler.ThrowOnFailure(hr);
			}
			finally
			{
				if (handle.IsAllocated)
					handle.Free();
			}
		}

		#endregion

		#region IEnumerable Members

		IEnumerator IEnumerable.GetEnumerator()
		{
			throw new NotImplementedException("The method or operation is not implemented.");
		}

		#endregion
	}
}