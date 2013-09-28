using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using System.Timers;
using System.Windows.Media.Animation;
using Expression.Samples.Interactivity;
using Microsoft.Expression.Interactivity.Media;

namespace FsmTester
{
    /// <summary>
    /// Логика взаимодействия для MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {

        //static double diameter = 0.6;
        //static double C = Math.PI * diameter;
        Storyboard story;

        public double Speed
        {
            set
            {
                var old = story.SpeedRatio;
                var news = value / old;
                story.SetSpeedRatio(news);
            }
        }

        public MainWindow()
        {
            InitializeComponent();
            

            story = (Storyboard)canvas1.FindResource("spin");
            story.Begin();
            story.SetSpeedRatio(0);

            var evt = new DataEventTrigger();
            var evt_bind = new Binding();
            evt_bind.Mode = BindingMode.OneWay;
            evt_bind.Source = DataContext;

            evt.Source = evt_bind;
            evt.EventName = "Started";
            evt.Attach(button);

            var play = new PlaySoundAction();
            play.Source = new Uri("Car Starting.wav", UriKind.Relative);

            evt.Actions.Add(play);
            Closing += (s, e) => CarViewModelLocator.Cleanup();

            var prop = DependencyProperty.Register("Speed", typeof(double), typeof(MainWindow));
            var binding = new Binding("Speed");
            binding.Mode = BindingMode.OneWay;
            binding.Source = DataContext;
            BindingOperations.SetBinding(this, prop, binding);
        }

    }
}
