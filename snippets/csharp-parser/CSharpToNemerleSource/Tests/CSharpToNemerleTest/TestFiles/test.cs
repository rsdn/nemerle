using System;
using System.Collections.Generic;
using System.Collections;
using System.Linq;
using System.Text;
using CommonLib.Linq;
using System.IO;
using CommonLib.Serialization;
using System.Configuration;
//using System.Windows.Forms;
//using CommonLib.utils.patterns;

namespace CommonLib.Utils
{
    using abc = List<int>;
    /// <summary>
    /// интерфейс определяющий что класс должен клонировать свои данные
    /// </summary>
    /// <typeparam name="TSource">тип класса</typeparam>
    public interface IDataCloned<TSource>
    {
        TSource DataClone();
    }

    public interface IDeepCloned<TSource>
    {
        TSource DeepClone();
    }

    /// <summary>
    /// Generic arguments class to pass to event handlers that need to receive data.
    /// </summary>
    /// <typeparam name="TData">The type of data to pass.</typeparam>
    public class DataEventArgs<TData> : EventArgs
    {
        TData data;

        /// <summary>
        /// Initializes the DataEventArgs class.
        /// </summary>
        /// <param name="data">Information related to the event.</param>
        /// <exception cref="ArgumentNullException">The data is null.</exception>
        public DataEventArgs(TData data)
        {
            if (data == null)
            {
                throw new ArgumentNullException("data");
            }
            this.data = data;
        }

        /// <summary>
        /// Gets the information related to the event.
        /// </summary>
        public TData Data
        {
            get { return data; }
        }

        /// <summary>
        /// Provides a string representation of the argument data.
        /// </summary>
        public override string ToString()
        {
            return data.ToString();
        }
    }

    [AttributeUsage(AttributeTargets.Property, Inherited = true, AllowMultiple = false)]
    public class Indexer : Attribute
    {
    }

#if NOT_USE_NEMERLE
    public static class Crc
    {
        private static readonly ushort[] crcTable = {
            0X0000, 0XC0C1, 0XC181, 0X0140, 0XC301, 0X03C0, 0X0280, 0XC241,
            0XC601, 0X06C0, 0X0780, 0XC741, 0X0500, 0XC5C1, 0XC481, 0X0440,
            0XCC01, 0X0CC0, 0X0D80, 0XCD41, 0X0F00, 0XCFC1, 0XCE81, 0X0E40,
            0X0A00, 0XCAC1, 0XCB81, 0X0B40, 0XC901, 0X09C0, 0X0880, 0XC841,
            0XD801, 0X18C0, 0X1980, 0XD941, 0X1B00, 0XDBC1, 0XDA81, 0X1A40,
            0X1E00, 0XDEC1, 0XDF81, 0X1F40, 0XDD01, 0X1DC0, 0X1C80, 0XDC41,
            0X1400, 0XD4C1, 0XD581, 0X1540, 0XD701, 0X17C0, 0X1680, 0XD641,
            0XD201, 0X12C0, 0X1380, 0XD341, 0X1100, 0XD1C1, 0XD081, 0X1040,
            0XF001, 0X30C0, 0X3180, 0XF141, 0X3300, 0XF3C1, 0XF281, 0X3240,
            0X3600, 0XF6C1, 0XF781, 0X3740, 0XF501, 0X35C0, 0X3480, 0XF441,
            0X3C00, 0XFCC1, 0XFD81, 0X3D40, 0XFF01, 0X3FC0, 0X3E80, 0XFE41,
            0XFA01, 0X3AC0, 0X3B80, 0XFB41, 0X3900, 0XF9C1, 0XF881, 0X3840,
            0X2800, 0XE8C1, 0XE981, 0X2940, 0XEB01, 0X2BC0, 0X2A80, 0XEA41,
            0XEE01, 0X2EC0, 0X2F80, 0XEF41, 0X2D00, 0XEDC1, 0XEC81, 0X2C40,
            0XE401, 0X24C0, 0X2580, 0XE541, 0X2700, 0XE7C1, 0XE681, 0X2640,
            0X2200, 0XE2C1, 0XE381, 0X2340, 0XE101, 0X21C0, 0X2080, 0XE041,
            0XA001, 0X60C0, 0X6180, 0XA141, 0X6300, 0XA3C1, 0XA281, 0X6240,
            0X6600, 0XA6C1, 0XA781, 0X6740, 0XA501, 0X65C0, 0X6480, 0XA441,
            0X6C00, 0XACC1, 0XAD81, 0X6D40, 0XAF01, 0X6FC0, 0X6E80, 0XAE41,
            0XAA01, 0X6AC0, 0X6B80, 0XAB41, 0X6900, 0XA9C1, 0XA881, 0X6840,
            0X7800, 0XB8C1, 0XB981, 0X7940, 0XBB01, 0X7BC0, 0X7A80, 0XBA41,
            0XBE01, 0X7EC0, 0X7F80, 0XBF41, 0X7D00, 0XBDC1, 0XBC81, 0X7C40,
            0XB401, 0X74C0, 0X7580, 0XB541, 0X7700, 0XB7C1, 0XB681, 0X7640,
            0X7200, 0XB2C1, 0XB381, 0X7340, 0XB101, 0X71C0, 0X7080, 0XB041,
            0X5000, 0X90C1, 0X9181, 0X5140, 0X9301, 0X53C0, 0X5280, 0X9241,
            0X9601, 0X56C0, 0X5780, 0X9741, 0X5500, 0X95C1, 0X9481, 0X5440,
            0X9C01, 0X5CC0, 0X5D80, 0X9D41, 0X5F00, 0X9FC1, 0X9E81, 0X5E40,
            0X5A00, 0X9AC1, 0X9B81, 0X5B40, 0X9901, 0X59C0, 0X5880, 0X9841,
            0X8801, 0X48C0, 0X4980, 0X8941, 0X4B00, 0X8BC1, 0X8A81, 0X4A40,
            0X4E00, 0X8EC1, 0X8F81, 0X4F40, 0X8D01, 0X4DC0, 0X4C80, 0X8C41,
            0X4400, 0X84C1, 0X8581, 0X4540, 0X8701, 0X47C0, 0X4680, 0X8641,
            0X8201, 0X42C0, 0X4380, 0X8341, 0X4100, 0X81C1, 0X8081, 0X4040
        };


        static int CalcCrcByte(byte ch, int crc)
        {
            byte tableIndex = (byte)(crc ^ ch);
            crc >>= 8;
            crc ^= crcTable[tableIndex];
            return crc;
        }

        public static ushort CalcCRC16(byte[] buf, int crc)
        {
            buf.Generate(ch => crc = CalcCrcByte(ch, crc));
            return (ushort)crc;
        }
    }

    public static class Hex
    {
        //        const string hexsymbs = "0123456789ABCDEF";

        //static void ConvTo(byte val,out char first,out char second)
        //{
        //    //char[] res = new char[2];
        //    second = hexsymbs[Common.LowQuat(val)];
        //    first = hexsymbs[Common.HighQuat(val)];
        //}

        public static string ConvertTo(byte[] val)
        {
            return new string(val.SelectMany(x => x.ToString("X2")).ToArray());
        }

        static byte hex2symb(byte hex)
        {
            if (hex >= 0x41) hex -= 0x37;
            else
                hex -= 0x30;
            return hex;
        }

        static byte ConvFrom(char first, char second)
        {
            byte low = hex2symb((byte)first);
            byte high = hex2symb((byte)second);//(byte)hexsymbs.IndexOf(val[1]);
            return Common.MakeByte(low, high);

        }

        public static byte[] ConvertFrom(string hex)
        {
            //char[] buf = hex.ToCharArray();
            if ((hex.Length & 1) == 1) return new byte[0];
            byte[] buf = new byte[hex.Length / 2];
            int idx = 0;
            using (CharEnumerator enumerator = hex.GetEnumerator())
            {
                while (enumerator.MoveNext())
                {
                    char first = enumerator.Current;
                    enumerator.MoveNext();
                    char second = enumerator.Current;
                    buf[idx++] = ConvFrom(second, first);
                }
            }
            //var query = hex.TakePartsIterator(2).Select(x => Convert.ToByte(x, 16));
            //byte[] outbuf = query.ToArray();
            return buf;
        }
    }

#endif

    public class ValidNum
    {
        double value;

        public ValidNum()
        {
            //value = 0;
        }

        public ValidNum(double val)
        {
            value = val;
            Valid = true;
        }

        public ValidNum(ValidNum num)
        {
            value = num.value;
            Valid = num.Valid;
            NotValidUsed = num.NotValidUsed;
        }

        [TreatAsFloat]
        public double Value
        {
            get
            {
                CheckValid();
                return this.value;
            }
            set
            {
                this.value = value;
                Valid = true;
            }
        }

        public void Invalidate()
        {
            Valid = false;
        }

        void CheckValid()
        {
            if (!Valid)
                if (NotValidUsed != null) NotValidUsed(this, null);
                else throw new InvalidOperationException();

        }

        public override string ToString()
        {
            if (Valid) return Value.ToString(); else return "<невалидна>";
        }

        public static implicit operator double(ValidNum val)
        {
            return val.Value;
        }

        public bool Valid { get; set; }

        public event EventHandler NotValidUsed;
    }

    public static class Time
    {
        public static int YearDays(int year)
        {
            return DateTime.IsLeapYear(year) ? 366 : 365;
        }

        public const int SecInDay = 3600 * 24;

        public static ulong SecInDays(ulong days)
        {
            return SecInDay * days;
            //secinday = 3600l * 24;
        }

        public static DateTime FromPacked(uint time)
        {
            DateTime datetime = new DateTime(2000, 1, 1);
            return datetime.AddSeconds(time);
        }

        public static uint ToPacked(DateTime time)
        {
            //time = new DateTime(2000, 1, 28, 0, 0, 0);
            DateTime datetime = new DateTime(2000, 1, 1);
            TimeSpan span = time.Subtract(datetime);
            //double sec = span.TotalSeconds;
            return (uint)span.TotalSeconds;
            //long ticks = TimeSpan.TicksPerSecond;
            //uint calc_sec = (uint)(span.Ticks / ticks);
            //return calc_sec;
        }

        //void set_from_packed(DWORD packed)
        //{
        //    WORD y;
        //    for (y = 2000; ; y++)
        //    {
        //        WORD d = year_days(y);
        //        DWORD sec = sec_in_days(d);//d*secinday;
        //        if (packed < sec) break;
        //        packed -= sec;
        //    }
        //    tyear = y;
        //    WORD temp = static_cast<WORD>(packed / secinday); // число дней в текущем году
        //    BYTE m;
        //    for (m = 1; ; m++)
        //    {
        //        WORD d = get_month_days(m, tyear);
        //        if (temp < d) break;
        //        temp -= d;
        //        DWORD sec = sec_in_days(d);
        //        packed -= sec;
        //    }
        //    tmonth = m;
        //    tday = static_cast<BYTE>(temp);
        //    thour = static_cast<BYTE>(packed / 3600);
        //    temp = packed % 3600;
        //    tmin = temp / 60;
        //    tsec = temp % 60;
        //}


        //DWORD get_packed()
        //{
        //    DWORD packed = 0;
        //    WORD temp, d;
        //    if (tmonth == 0 || tyear == 0) return 0;
        //    for (temp = 2000; temp < tyear; temp++)
        //    {
        //        d = year_days(temp);
        //        packed += sec_in_days(d);
        //    }
        //    for (temp = 1; temp < tmonth; temp++)
        //    {
        //        d = get_month_days(static_cast<BYTE>(temp), tyear);
        //        packed += sec_in_days(d);
        //    }
        //    packed += sec_in_days(tday);
        //    packed += thour * 3600;
        //    packed += tmin * 60;
        //    packed += tsec;
        //    return packed;
        //}

    }

    public class SettingBind
    {
        ApplicationSettingsBase main, sub;

        public SettingBind(ApplicationSettingsBase base_set, ApplicationSettingsBase sub_set)
        {
            main = base_set;
            sub = sub_set;
            sub_set.SettingsSaving += new SettingsSavingEventHandler(sub_set_SettingsSaving);
        }

        public void Load()
        {
            main.CopyTo(sub);
        }

        void sub_set_SettingsSaving(object sender, System.ComponentModel.CancelEventArgs e)
        {
            sub.CopyTo(main);
            e.Cancel = true;
            main.Save();
        }


    }

    public static class SettingsUtils
    {
        public static void CopyTo(this ApplicationSettingsBase settings, ApplicationSettingsBase to_set)
        {
            //var vals=to_set.PropertyValues.Cast<SettingsPropertyValue>();
            var vals=settings.PropertyValues.Cast<SettingsPropertyValue>();
            vals.Generate(x => to_set[x.Name] = settings[x.Name]);
        }

        public static void BindTo(this ApplicationSettingsBase main, ApplicationSettingsBase sub)
        {

        }


        static void main_SettingsLoaded(object sender, SettingsLoadedEventArgs e)
        {

        }
    }
}