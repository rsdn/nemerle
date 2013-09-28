using GalaSoft.MvvmLight;
using GalaSoft.MvvmLight.Threading;
using GalaSoft.MvvmLight.Command;
using Nemerle.Statechart.Tests;
using System;
using System.Windows;
using System.Windows.Media;
using System.Windows.Controls;

namespace FsmTester
{
    /// <summary>
    /// This class contains properties that a View can data bind to.
    /// <para>
    /// Use the <strong>mvvminpc</strong> snippet to add bindable properties to this ViewModel.
    /// </para>
    /// <para>
    /// You can also use Blend to data bind with the tool's support.
    /// </para>
    /// <para>
    /// See http://www.galasoft.ch/mvvm/getstarted
    /// </para>
    /// </summary>
    public class CarViewModel : ViewModelBase
    {        
        CarFsm fsm = new CarFsm();
        /// <summary>
        /// Initializes a new instance of the MvvmViewModel1 class.
        /// </summary>
        public CarViewModel()
        {
            GasCommand = new RelayCommand(() => fsm.GAS_PEDAL());
            KeyCommand = new RelayCommand(() => fsm.TURN_KEY());
            BreakCommand = new RelayCommand(() => fsm.BREAK_PEDAL());
            fsm.PropertyChanged += new System.ComponentModel.PropertyChangedEventHandler(fsm_PropertyChanged);
            
            //fsm.ChangeBind("speed", () => window.Dispatcher.Invoke(new Action(() => window.SetSpeed(fsm.speed))));
            fsm.start += new Action(fsm_start);
            fsm.Initiate();
        }

        public event EventHandler Started;

        void fsm_start()
        {
            if (Started != null) DispatcherHelper.CheckBeginInvokeOnUI(() => Started(this, null));
        }

        void fsm_PropertyChanged(object sender, System.ComponentModel.PropertyChangedEventArgs e)
        {
            switch (e.PropertyName) 
            {
                case "speed": base.RaisePropertyChanged("Speed");break;
                case "CurState": base.RaisePropertyChanged("State"); break;
            }
        }

        public RelayCommand GasCommand { get; private set; }
        public RelayCommand KeyCommand { get; private set; }
        public RelayCommand BreakCommand { get; private set; }

        public string State
        {
            get
            {
                return fsm.CurState.ToString();
            }
        }

        public double Speed
        {
            get
            {
                return fsm.speed;
            }
        }

        public override void Cleanup()
        {
            // Clean own resources if needed
            fsm.Terminate();
            base.Cleanup();
        }
    }
}
