using System;
using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.TextManager.Interop;

using VSConstants = Microsoft.VisualStudio.VSConstants;
using ErrorHandler = Microsoft.VisualStudio.ErrorHandler;

namespace Nemerle.VisualStudio.LanguageService
{
	/// <summary>
	/// This partial class contains the code needed to make the view filter work when
	/// the language service is contained inside another language. The problem in this
	/// case is that there is no access to the actual text buffer, but we have to use
	/// the buffer coordinator that knows how to map the primary buffer to the secondary.
	/// </summary>
	internal partial class NemerleViewFilter
	{
		private IVsTextBufferCoordinator _bufferCoordinator;
		internal IVsTextBufferCoordinator BufferCoordinator
		{
			get { return _bufferCoordinator; }
			set { _bufferCoordinator = value; }
		}

		public override ExpansionProvider GetExpansionProvider()
		{
			if (null == this.Source)
			{
				return null;
			}
			return base.GetExpansionProvider();
		}

		// Override the IVsTextViewFilter methods.

		public override int GetWordExtent(int line, int index, uint flags, TextSpan[] span)
		{
			// Check if we have a buffer coordinator set.
			if (null == BufferCoordinator)
			{
				// No buffer coordinator, so delegate to the base implementation.
				return base.GetWordExtent(line, index, flags, span);
			}

			// Make sure that the span array is not empty.
			if ((null == span) || (span.Length == 0))
			{
				throw new ArgumentNullException("span");
			}

			// There is a buffer coordinator, so we have to translate the spans from the
			// primary to the secondary buffer.
			TextSpan originalSpan = new TextSpan();
			originalSpan.iStartLine = line;
			originalSpan.iStartIndex = index;
			originalSpan.iEndLine = line;
			originalSpan.iEndIndex = index;

			TextSpan[] convertedSpan = new TextSpan[1];
			ErrorHandler.ThrowOnFailure(BufferCoordinator.MapPrimaryToSecondarySpan(originalSpan, convertedSpan));
			// Now call the base function using the converted location.
			TextSpan[] secondarySpan = new TextSpan[1];
			int returnCode = base.GetWordExtent(convertedSpan[0].iStartLine, convertedSpan[0].iStartIndex, flags, secondarySpan);
			if (VSConstants.S_OK != returnCode)
			{
				return returnCode;
			}
			// Convert the returned span to something usable in the primary buffer.
			ErrorHandler.ThrowOnFailure(BufferCoordinator.MapSecondaryToPrimarySpan(secondarySpan[0], span));
			return VSConstants.S_OK;
		}

		public override int GetPairExtents(int line, int index, TextSpan[] span)
		{
			// If the buffer coordinator is null, then this is the standard case and we can
			// delegate to the base implementation.
			if (BufferCoordinator == null)
			{
				return base.GetPairExtents(line, index, span);
			}

			// Verify that the array with the text span is usable.
			if ((span == null) || (span.Length == 0))
			{
				throw new ArgumentNullException("span");
			}

			// Now we have to translate the position from the primary to the secondary buffer.
			TextSpan originalSpan = new TextSpan();
			originalSpan.iStartLine = line;
			originalSpan.iStartIndex = index;
			originalSpan.iEndLine = line;
			originalSpan.iEndIndex = index;

			TextSpan[] convertedSpan = new TextSpan[1];
			ErrorHandler.ThrowOnFailure(BufferCoordinator.MapPrimaryToSecondarySpan(originalSpan, convertedSpan));

			// Now we can call the base implementation saving the result in a temporary span that
			// is relative to the secondary buffer.
			TextSpan[] secondarySpan = new TextSpan[1];
			int returnCode = base.GetPairExtents(convertedSpan[0].iStartLine, convertedSpan[0].iStartIndex, secondarySpan);
			if (VSConstants.S_OK != returnCode)
			{
				return returnCode;
			}

			// Translate the span for the primary buffer.
			ErrorHandler.ThrowOnFailure(BufferCoordinator.MapSecondaryToPrimarySpan(secondarySpan[0], span));
			return VSConstants.S_OK;
		}

		public override void Dispose()
		{
			try
			{
				((NemerleLanguageService)Source.LanguageService).DisposeColorizer(this.Source.TextLines);

				_bufferCoordinator = null;

				// Базовая реализация base.Dispose() предполагает, что значение внутреннего поля textView 
				// является COM объектом и безусловно вызывает для него Marshal.ReleaseComObject.
				// Но для Contained Language в качестве view используется TextViewWrapper, который не является
				// COM объектом, что приводит к генерации исключения внутри base.Dispose() и 
				// преждевременному выходу из функции.
				// 
				// Поэтому деинициализаируем textView вручную и обнулим значение поля. Это отменит вызов ReleaseComObject

				// HACK: обнулим значение приватного поля textView, если его значение имеет тип TextViewWrapper
				var textViewFieldInfo = this.GetType().BaseType.GetField("textView", System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic);
				if (textViewFieldInfo != null)
				{
					var textView = (IVsTextView)textViewFieldInfo.GetValue(this);

					if (textView is TextViewWrapper)
					{
						textView.CloseView();
						textViewFieldInfo.SetValue(this, null);
					}
				}

				base.Dispose();
			}
			catch (Exception)
			{
			}
		}
	}
}
