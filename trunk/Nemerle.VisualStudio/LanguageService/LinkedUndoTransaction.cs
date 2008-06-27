using System;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.TextManager.Interop;

namespace Nemerle.VisualStudio.LanguageService
{
	internal class LinkedUndoTransaction : System.ComponentModel.Component
	{
		private readonly IVsLinkedUndoTransactionManager _mgr;

		public LinkedUndoTransaction(string description, IServiceProvider host)
			: this (LinkedTransactionFlags.mdtStrict, description, host)
		{
		}

		public LinkedUndoTransaction(LinkedTransactionFlags flags, string description, IServiceProvider host)
		{
			if (description == null)
				throw new ArgumentNullException("description");
			if (host == null)
				throw new ArgumentNullException("host");

			_mgr = (IVsLinkedUndoTransactionManager)host.GetService(typeof (IVsLinkedUndoTransactionManager));
			if (_mgr == null)
				throw new ArgumentNullException("host", "The host must provide IVsLinkedUndoTransactionManager service.");

			ErrorHandler.ThrowOnFailure(_mgr.OpenLinkedUndo((uint)flags, description));
			_isActive = true;
		}

		/// <summary>
		/// Commits a linked undo transaction.
		/// </summary>
		public void Commit()
		{
			if (!IsActive)
				throw new InvalidOperationException("This operation can only be performed when a transaction is active.");

			ErrorHandler.ThrowOnFailure(_mgr.CloseLinkedUndo());
			_isActive = false;
		}

		/// <summary>
		/// Aborts a linked undo transaction.
		/// </summary>
		public void Rollback()
		{
			if (!IsActive)
				throw new InvalidOperationException("This operation can only be performed when a transaction is active.");

			ErrorHandler.ThrowOnFailure(_mgr.AbortLinkedUndo());
			_isActive = false;
		}

		private bool _isActive;
		/// <summary>
		/// Determines if the linked undo transaction is active.
		/// </summary>
		public  bool  IsActive
		{
			get
			{
				if (_isActive)
				{
					int aborted = 0;
					ErrorHandler.ThrowOnFailure(_mgr.IsAborted(ref aborted));
					return aborted == 0;
				}
				return false;
			}
		}

		///<summary>
		///Releases the unmanaged resources used by the <see cref="T:System.ComponentModel.Component"></see> and optionally releases the managed resources.
		///</summary>
		///
		///<param name="disposing">true to release both managed and unmanaged resources; false to release only unmanaged resources. </param>
		protected override void Dispose(bool disposing)
		{
			if (IsActive)
				Rollback();

			base.Dispose(disposing);
		}
	}
}