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
using Nemerle.Statechart;

using NST = Nemerle.Statechart.Tests;

namespace Calculator.Windows
{
    /// <summary>
    /// Логика взаимодействия для MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        NST.Calculator fsm = new NST.Calculator();

        public MainWindow()
        {
            InitializeComponent();
            fsm.Show += x => Dispatcher.Invoke(new Action<string>(fsm_Show), x);
            fsm.TransitionCompleted += (x, y) => Dispatcher.Invoke(new Action(fsm_TransitionCompleted));
        }

        void fsm_TransitionCompleted()
        {
            if (statusBar.Items.Count == 2) statusBar.Items.RemoveAt(1);
            statusBar.Items.Add(fsm.ToString());
        }

        protected override void OnClosed(EventArgs e)
        {
            base.OnClosed(e);
            fsm.Terminate();
        }

        protected override void OnContentRendered(EventArgs e)
        {
            base.OnContentRendered(e);
            fsm.Initiate();
        }

        void fsm_Show(string str)
        {
            screen.Text = str;
        }

        private void one_Click(object sender, RoutedEventArgs e)
        {
            fsm.digit('1');
        }

        private void two_Click(object sender, RoutedEventArgs e)
        {
            fsm.digit('2');
        }

        private void three_Click(object sender, RoutedEventArgs e)
        {
            fsm.digit('3');
        }

        private void zero_Click(object sender, RoutedEventArgs e)
        {
            fsm.digit('0');
        }

        private void four_Click(object sender, RoutedEventArgs e)
        {
            fsm.digit('4');
        }

        private void five_Click(object sender, RoutedEventArgs e)
        {
            fsm.digit('5');
        }

        private void six_Click(object sender, RoutedEventArgs e)
        {
            fsm.digit('6');
        }

        private void seven_Click(object sender, RoutedEventArgs e)
        {
            fsm.digit('7');
        }

        private void eight_Click(object sender, RoutedEventArgs e)
        {
            fsm.digit('8');
        }

        private void nine_Click(object sender, RoutedEventArgs e)
        {
            fsm.digit('9');
        }

        private void point_Click(object sender, RoutedEventArgs e)
        {
            fsm.point();
        }

        private void plus_Click(object sender, RoutedEventArgs e)
        {
            fsm.oper('+');
        }

        private void minus_Click(object sender, RoutedEventArgs e)
        {
            fsm.oper('-');
        }

        private void multiply_Click(object sender, RoutedEventArgs e)
        {
            fsm.oper('*');
        }

        private void divide_Click(object sender, RoutedEventArgs e)
        {
            fsm.oper('/');
        }

        private void equals_Click(object sender, RoutedEventArgs e)
        {
            fsm.equal();
        }

        private void one_divide_Click(object sender, RoutedEventArgs e)
        {
            fsm.oper('1');
        }

        private void square_root_Click(object sender, RoutedEventArgs e)
        {
            fsm.oper('s');
        }

        private void C_button_Click(object sender, RoutedEventArgs e)
        {
            fsm.C();
        }

        private void CE_button_Click(object sender, RoutedEventArgs e)
        {
            fsm.CE();
        }

        private void button1_Click(object sender, RoutedEventArgs e)
        {
            fsm.off();
        }

        private void button2_Click(object sender, RoutedEventArgs e)
        {
            fsm.oper('%');
        }

        private void Window_TextInput(object sender, TextCompositionEventArgs e)
        {
            var evt = NST.Calculator.GetEventFromStr(e.Text);
            if (evt.HasValue)
            {
                fsm.PostEvent(evt.Value);
                e.Handled = true;
            }
        }

        private void Window_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.Key == Key.Escape)
            {
                fsm.CE();
                e.Handled = true;
            } else
                if (e.Key == Key.Return)
                {
                    fsm.equal();
                    e.Handled = true;
                }
        }


    }
}
