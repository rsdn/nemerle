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
using Nemerle.Statechart.Tests;
using System.Threading;
using System.Threading.Tasks;

namespace AlarmClockWindow
{
    /// <summary>
    /// Логика взаимодействия для MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        AlarmClock fsm = new AlarmClock();
        FastKeystroke key_stroke_hour = new FastKeystroke();
        FastKeystroke key_stroke_min = new FastKeystroke();

        public MainWindow()
        {
            InitializeComponent();
            fsm.TransitionCompleted += (x, y) => Dispatcher.Invoke(new Action(fsm_TransitionCompleted));
            fsm.set_am += () => Dispatcher.Invoke(new Action(fsm_set_am));
            fsm.set_pm += () => Dispatcher.Invoke(new Action(fsm_set_pm));
            fsm.Show += x => Dispatcher.Invoke(new Action<string>(fsm_Show), x);
            fsm.al_icon_off += () => Dispatcher.Invoke(new Action(fsm_al_icon_off));
            fsm.al_icon_on += () => Dispatcher.Invoke(new Action(fsm_al_icon_on));
            fsm.light_off += () => Dispatcher.Invoke(new Action(fsm_light_off));
            fsm.light_on += () => Dispatcher.Invoke(new Action(fsm_light_on));
            fsm.alarm_icon_off += () => Dispatcher.Invoke(new Action(fsm_alarm_icon_off));
            fsm.alarm_icon_on += () => Dispatcher.Invoke(new Action(fsm_alarm_icon_on));
            key_stroke_hour.holded += new Action(key_stroke_hour_holded);
            key_stroke_hour.released += new Action(key_stroke_hour_released);
            key_stroke_min.holded += new Action(key_stroke_min_holded);
            key_stroke_min.released += new Action(key_stroke_min_released);
            key_stroke_hour.Initiate();
            key_stroke_min.Initiate();
            fsm.Initiate();
        }

        void fsm_alarm_icon_on()
        {
            signal_image.Source = new BitmapImage(new Uri(@"pack://application:,,,/AlarmClock;component/Images/alarm2.png"));
        }

        void fsm_alarm_icon_off()
        {
            signal_image.Source = null;
        }

        void fsm_light_on()
        {
            background.Fill = Brushes.LightGoldenrodYellow;
        }

        void fsm_light_off()
        {
            background.Fill = new SolidColorBrush(Color.FromRgb(0xE6, 0xE6, 0xE6));
        }

        void key_stroke_min_released()
        {
            fsm.release_min();
        }

        void key_stroke_hour_released()
        {
            fsm.release_hour();
        }

        void key_stroke_min_holded()
        {
            fsm.hold_min();
        }

        void key_stroke_hour_holded()
        {
            fsm.hold_hour();
        }

        void fsm_al_icon_on()
        {
            alarm_sign.Text = "AL";
        }

        void fsm_al_icon_off()
        {
            alarm_sign.Text = "";
        }

        void fsm_Show(string obj)
        {
            display.Text = obj;
        }

        void fsm_set_pm()
        {
            am_pm_sign.Text = "PM";
        }

        void fsm_set_am()
        {
            am_pm_sign.Text = "AM";
        }

        void send_hour(CancellationToken tok)
        {
            while (!tok.IsCancellationRequested)
            {
                fsm.push_hour();
                Thread.Sleep(10);
            }
        }

        void send_min(CancellationToken tok)
        {
            while (!tok.IsCancellationRequested)
            {
                fsm.push_min();
                Thread.Sleep(10);
            }
        }

        void fsm_TransitionCompleted()
        {
            if (status.Items.Count == 2) status.Items.RemoveAt(1);
            status.Items.Add(fsm.ToString());
        }

        private void mode_slider_ValueChanged(object sender, RoutedPropertyChangedEventArgs<double> e)
        {
            switch ((int)e.NewValue)
            {
                case 0: fsm.time_set(); break;
                case 1: fsm.run(); break;
                case 2: fsm.alarm_set(); break;
            }
            e.Handled = true;
        }
        
        private void hour_button_Click(object sender, RoutedEventArgs e)
        {
            fsm.push_hour();
        }

        private void minute_button_Click(object sender, RoutedEventArgs e)
        {
            fsm.push_min();
        }

        private void hour_button_PreviewMouseDown(object sender, MouseButtonEventArgs e)
        {
            key_stroke_hour.push();
        }

        private void minute_button_PreviewMouseDown(object sender, MouseButtonEventArgs e)
        {
            key_stroke_min.push();
        }

        private void hour_button_PreviewMouseUp(object sender, MouseButtonEventArgs e)
        {
            key_stroke_hour.release();
        }

        private void minute_button_PreviewMouseUp(object sender, MouseButtonEventArgs e)
        {
            key_stroke_min.release();
        }

        private void alarm_on_radio_Checked(object sender, RoutedEventArgs e)
        {
            fsm.alarm_on();
        }

        private void alarm_off_radio_Checked(object sender, RoutedEventArgs e)
        {
            fsm.alarm_off();
        }

        private void drowse_light_button_PreviewMouseDown(object sender, MouseButtonEventArgs e)
        {
            fsm.drowse_light();
        }

        private void drowse_light_button_PreviewMouseUp(object sender, MouseButtonEventArgs e)
        {
            fsm.drowse_light_release();
        }


    }
}
