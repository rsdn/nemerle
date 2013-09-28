using System;
using System.Collections.Generic;
using System.Configuration;
using System.Data;
using System.Linq;
using System.Windows;
using GalaSoft.MvvmLight.Threading;

namespace FsmTester
{
    /// <summary>
    /// Логика взаимодействия для App.xaml
    /// </summary>
    public partial class App : Application
    {

        static App()
        {
            DispatcherHelper.Initialize();
        }

        

        //void App_Exit(object sender, ExitEventArgs e)
        //{
        //    car.Stop();
        //}

        //void win_Loaded(object sender, RoutedEventArgs e)
        //{
        //    car.Init();
        //}
    }
}
