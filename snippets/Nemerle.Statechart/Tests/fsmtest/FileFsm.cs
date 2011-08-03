using System;
using System.Diagnostics;
using Sanford.StateMachineToolkit;
using System.Windows.Forms;
using CommonLib.Fsm;

namespace CommonLib.Fsm.FileFsm
{

    public enum StateID
    {
        Files,
        Waiting,
        Opened,
        NotSaved,
        New,
        Changed,
        Saved,
        Closed,
        Closing,
        //=========
        Processing,
        CreatingNew,
        NewDialog,
        SavingNew,
        SaveClose,        
        //==========
        SaveFile,
        SaveFileAs,
        OpeningFile,
        SavingOpen,
        OpenDialog,
    };

    public enum EventID
    {
        Changed,
        Cancel,
        NewFile,
        OpenFile,
        SaveFile,
        SaveFileAs,
        Exit,

        //===========
        //evt_Final,
        evt_Completion
    };

    [DebuggerDisplay("State = {CurrentStateID}")]
    public class FileFsm : StateMachine<StateID,EventID>
    {
                
        public FileFsm()
        {            
            this[StateID.Waiting].EntryHandler+=new EventHandler<TransitionEventArgs<StateID,EventID,EventArgs>>(FileFsm_WaitingEntryHandler);
            this[StateID.NotSaved].EntryHandler += new EventHandler<TransitionEventArgs<StateID, EventID, EventArgs>>(FileFsm_NotSavedEntryHandler);
            this[StateID.New].EntryHandler += new EventHandler<TransitionEventArgs<StateID, EventID, EventArgs>>(FileFsm_NewEntryHandler);
            this[StateID.Changed].EntryHandler += new EventHandler<TransitionEventArgs<StateID, EventID, EventArgs>>(FileFsm_ChangedEntryHandler);
            this[StateID.Saved].EntryHandler += new EventHandler<TransitionEventArgs<StateID, EventID, EventArgs>>(FileFsm_SavedEntryHandler);
            this[StateID.SavingNew].EntryHandler+=new EventHandler<TransitionEventArgs<StateID,EventID,EventArgs>>(FileFsm_SavingEntryHandler);
            this[StateID.NewDialog].EntryHandler += new EventHandler<TransitionEventArgs<StateID, EventID, EventArgs>>(FileFsm_NewDialogEntryHandler);
            this[StateID.SaveFile].EntryHandler += new EventHandler<TransitionEventArgs<StateID, EventID, EventArgs>>(FileFsm_SaveFileEntryHandler);
            this[StateID.SaveFileAs].EntryHandler += new EventHandler<TransitionEventArgs<StateID, EventID, EventArgs>>(FileFsm_SaveFileAsEntryHandler);
            this[StateID.SavingOpen].EntryHandler += new EventHandler<TransitionEventArgs<StateID, EventID, EventArgs>>(FileFsm_SavingEntryHandler);
            this[StateID.OpenDialog].EntryHandler += new EventHandler<TransitionEventArgs<StateID, EventID, EventArgs>>(FileFsm_OpenDialogEntryHandler);
            this[StateID.SaveClose].EntryHandler += new EventHandler<TransitionEventArgs<StateID, EventID, EventArgs>>(FileFsm_SavingEntryHandler);
            this[StateID.Opened].EntryHandler += new EventHandler<TransitionEventArgs<StateID, EventID, EventArgs>>(FileFsm_OpenedEntryHandler);
            this[StateID.Closed].EntryHandler += new EventHandler<TransitionEventArgs<StateID, EventID, EventArgs>>(FileFsm_ExitedEntryHandler);
            SetupSubstates(StateID.Files, HistoryType.Deep, StateID.Waiting, StateID.Opened);
            SetupSubstates(StateID.Opened, HistoryType.None, StateID.NotSaved, StateID.Saved);
            SetupSubstates(StateID.NotSaved, HistoryType.None, StateID.New, StateID.Changed);
            SetupSubstates(StateID.Processing, HistoryType.None, 
                StateID.CreatingNew,
                StateID.SaveFile,
                StateID.SaveFileAs,
                StateID.OpeningFile,
                StateID.Closing
                );            
            SetupSubstates(StateID.CreatingNew, HistoryType.None, StateID.SavingNew, StateID.NewDialog);
            SetupSubstates(StateID.OpeningFile, HistoryType.None, StateID.SavingOpen, tateID.OpenDialog);
            SetupSubstates(StateID.Closing, HistoryType.None, StateID.SaveClose, tateID.Closed);

            AddTransition(StateID.Saved, EventID.Changed, StateID.Changed);
            AddTransition(StateID.Files, EventID.NewFile, StateID.Processing);
            AddTransition(StateID.Files, EventID.OpenFile, StateID.OpeningFile);
            AddTransition(StateID.Opened, EventID.SaveFile, StateID.SaveFile);
            AddTransition(StateID.Opened, EventID.SaveFileAs, StateID.SaveFileAs);
            AddTransition(StateID.Files, EventID.NewFile, StateID.Processing);
            AddTransition(StateID.SavingNew, EventID.evt_Completion, StateID.NewDialog);
            AddTransition(StateID.CreatingNew, EventID.evt_Completion, StateID.Opened);
            AddTransition(StateID.Processing, EventID.Cancel, StateID.Files);
            AddTransition(StateID.Processing, EventID.evt_Completion, StateID.Saved);
            AddTransition(StateID.SavingOpen, EventID.evt_Completion, StateID.OpenDialog);
            AddTransition(StateID.SaveClose, EventID.evt_Completion, StateID.Closed);
            AddTransition(StateID.Files, EventID.Exit, StateID.Closing);
            CurWantToSave = WantToSaveString;
            CurOverwrite = WantToOverwriteString;
            base.TransitionCompleted += new EventHandler<TransitionCompletedEventArgs<StateID, EventID, EventArgs>>(FileFsm_TransitionCompleted);
        }

        void FileFsm_ExitedEntryHandler(object sender, TransitionEventArgs<StateID, EventID, EventArgs> e)
        {
            OnExitAction();
        }

        void FileFsm_OpenedEntryHandler(object sender, TransitionEventArgs<StateID, EventID, EventArgs> e)
        {
            if (SaveAsEna != null) SaveAsEna();
            OnOpenedEntry();
        }

        void OnOpenedEntry()
        {
            if (OpenedEntry != null) OpenedEntry();
        }

        void FileFsm_SaveFileAsEntryHandler(object sender, TransitionEventArgs<StateID, EventID, EventArgs> e)
        {
            SaveAsDialog();
        }

        void FileFsm_SavedEntryHandler(object sender, TransitionEventArgs<StateID, EventID, EventArgs> e)
        {
            LastSavedFile = CurFile;
            if (SaveDis != null) SaveDis();
            if (SavedEntry != null) SavedEntry();            
        }

        void FileFsm_ChangedEntryHandler(object sender, TransitionEventArgs<StateID, EventID, EventArgs> e)
        {
            if (ChangedEntry != null) ChangedEntry();
        }

        void FileFsm_NewEntryHandler(object sender, TransitionEventArgs<StateID, EventID, EventArgs> e)
        {            
            if (NewEntry != null) NewEntry();
            LastSavedFile = "";

        }

        void FileFsm_NotSavedEntryHandler(object sender, TransitionEventArgs<StateID, EventID, EventArgs> e)
        {
            if (SaveEna != null) SaveEna();
        }

        void FileFsm_WaitingEntryHandler(object sender, TransitionEventArgs<StateID, EventID, EventArgs> e)
        {
            if (SaveDis != null) SaveDis();
            if (SaveAsDis != null) SaveAsDis();
            if (WaitEntry != null) WaitEntry();
        }

        void FileFsm_OpenDialogEntryHandler(object sender, TransitionEventArgs<StateID, EventID, EventArgs> e)
        {
            DialogResult res;
            res = ViewOpenDialog();
            if (res == DialogResult.Cancel)
            {
                CancelEvent();
                return;
            }
            CurFile = OpenDialog.FileName;
            if (OpenFileAction != null)
            {
                CompleteOrCancel(OpenFileAction(CurFile));
            }
            else throw new InvalidOperationException();
            
        }

        void FileFsm_SaveFileEntryHandler(object sender, TransitionEventArgs<StateID, EventID, EventArgs> e)
        {
            DoSaveFile();
        }

        void FileFsm_NewDialogEntryHandler(object sender, TransitionEventArgs<StateID, EventID, EventArgs> e)
        {
            if (NewFileAction != null)
            {                
                CompleteOrCancel(NewFileAction());                
            }
            else Completion();
        }


        void FileFsm_SavingEntryHandler(object sender, TransitionEventArgs<StateID, EventID, EventArgs> e)
        {
            DoSaving();
        }

        void DoSaving()
        {
            DialogResult res;
            if (IsNeedSave)
            {
                res = ViewWantToSave();
                if (res == DialogResult.Cancel)
                {
                    CancelEvent();
                    return;
                }
                if (res == DialogResult.Yes)
                {
                    DoSaveFile();
                }
                else Completion();
            }
            else Completion();
        }

        void DoSaveFile()
        {
            DialogResult res;
            if (IsNewFile)
            {
                SaveAsDialog();
            }
            else
            {
                res = ViewOverwrite();
                if (res == DialogResult.Cancel)
                {
                    CancelEvent();
                    return;
                }
                if (res == DialogResult.Yes)
                {
                    CurFile = LastSavedFile;
                    SaveCurrentFile();
                }
                else
                    SaveAsDialog();
            }
        }

        void OnExitAction()
        {
            if (ExitAction != null) ExitAction();
        }

        void CompleteOrCancel(bool val)
        {
            if (val) Completion(); else CancelEvent();
        }

        void SaveCurrentFile()
        {
            if (SaveFileAction != null)
            {
                CompleteOrCancel(SaveFileAction(CurFile));                
            }
            else Completion();
        }

        void SaveAsDialog()
        {
            DialogResult res;
            SaveDialog.FileName = CurFile;
            res = ViewSaveAsDialog();
            if (res == DialogResult.OK)
            {
                CurFile = SaveDialog.FileName;
                SaveCurrentFile();
            }
            else CancelEvent();
        }

        void CancelEvent()
        {            
            base.Send(EventID.Cancel);
        }

        void Completion()
        {       
            base.Send(EventID.evt_Completion);                        
        }

        void FileFsm_TransitionCompleted(object sender, TransitionCompletedEventArgs<StateID, EventID, EventArgs> e)
        {
            base.Execute();
        }

        void FileFsm_TransitionDeclined(object sender, TransitionEventArgs<StateID, EventID, EventArgs> e)
        {
            base.Execute();
        }

        public const string WantToSaveString = "Вы не сохранили текущий файл, Хотите сохранить?";
        public const string WantToOverwriteString = "Файл будет перезаписан, вы уверены?";
        /// <summary>
        /// текущая строка хотите сохранить текущий файл
        /// </summary>
        public string CurWantToSave { get; set; }
        /// <summary>
        /// текущая строка хотите перезаписать
        /// </summary>
        public string CurOverwrite { get; set; }

        /// <summary>
        /// текущий диалог сохранения
        /// </summary>
        public SaveFileDialog SaveDialog { get; set; }
        /// <summary>
        /// текущий диалог открытия
        /// </summary>
        public OpenFileDialog OpenDialog { get; set; }

        /// <summary>
        /// текущий файл из диалога
        /// </summary>
        public string CurFile { get; private set; }

        /// <summary>
        /// последний сохраненный файл
        /// </summary>
        public string LastSavedFile { get; private set; }

        DialogResult ViewOpenDialog()
        {
            if (OpenDialog != null)
                return OpenDialog.ShowDialog();
            else throw new InvalidOperationException();
        }

        DialogResult ViewWantToSave()
        {
            DialogResult res=MessageBox.Show(CurWantToSave, "Предупреждение", MessageBoxButtons.YesNoCancel);
            return res;
        }

        DialogResult ViewSaveAsDialog()
        {
            if (SaveDialog != null)
                return SaveDialog.ShowDialog();
            else throw new InvalidOperationException();
        }

        DialogResult ViewOverwrite()
        {
            return MessageBox.Show(CurOverwrite, "Предупреждение", MessageBoxButtons.YesNoCancel);
        }


        public void Init()
        {
            Initialize(StateID.Files);
        }

        public bool IsNeedSave
        {
            get
            {                       
                return base.IsInState(StateID.NotSaved);
            }
        }

        /// <summary>
        /// показывает что файл новый или открыт
        /// </summary>
        public bool IsOpened
        {
            get
            {
                return base.IsInState(StateID.Opened);
            }
        }

        bool IsNewFile
        {
            get
            {                
                return CurrentStateID == StateID.New;
            }
        }

        public bool IsClosed
        {
            get
            {
                return base.IsInState(StateID.Closing);//CurrentStateID == StateID.Closed;
            }
        }

        public event System.Func<string,bool> SaveFileAction;
        public event System.Func<bool> NewFileAction;
        public event System.Func<string,bool> OpenFileAction;

        public event Action SaveAsEna;
        public event Action SaveAsDis;
        public event Action SaveDis;
        public event Action SaveEna;
        public event Action ChangedEntry;
        public event Action NewEntry;
        public event Action SavedEntry;
        public event Action WaitEntry;
        public event Action OpenedEntry;

        public event Action ExitAction;
				
    }

}
