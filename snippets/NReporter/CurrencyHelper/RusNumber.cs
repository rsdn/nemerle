using System;
using System.Text;

namespace Rsdn.Janus.Framework
{
	/// <summary>
	/// A set of C# classes for spelling Russian numerics 
	/// Copyright (c) 2002 Rsdn Group
	/// </summary>
	public static class RusNumber
	{
		private static readonly string[] _frac20Male =
			new[]
			{
				"", "один", "два", "три", "четыре", "пять", "шесть",
				"семь", "восемь", "девять", "десять", "одиннадцать",
				"двенадцать", "тринадцать", "четырнадцать", "пятнадцать",
				"шестнадцать", "семнадцать", "восемнадцать", "девятнадцать"
			};

		private static readonly string[] _frac20Female =
			new[]
			{
				"", "одна", "две", "три", "четыре", "пять", "шесть",
				"семь", "восемь", "девять", "десять", "одиннадцать",
				"двенадцать", "тринадцать", "четырнадцать", "пятнадцать",
				"шестнадцать", "семнадцать", "восемнадцать", "девятнадцать"
			};

		private static readonly string[] _hunds =
			{
				"", "сто", "двести", "триста", "четыреста",
				"пятьсот", "шестьсот", "семьсот", "восемьсот", "девятьсот"
			};

		private static readonly string[] _tens =
			{
				"", "десять", "двадцать", "тридцать", "сорок", "пятьдесят",
				"шестьдесят", "семьдесят", "восемьдесят", "девяносто"
			};


		public static string ToRubles(this decimal value)
		{
			var rubVal = Math.Truncate(value);
			var kopVal = (value - rubVal) * 100;
			var rubResult = rubVal.RusSpelledOut(true, "рубль", "рубля", "рублей");
			rubResult = rubResult.ToUpperFirstChar();
			if (kopVal > 0)
			{
				var kopResult = kopVal.RusSpelledOut(false, "копейка", "копейки", "копеек");
				return rubResult + " " + kopResult;
			}
			else
				return rubResult;
		}

		private static string ToUpperFirstChar(this string rubResult)
		{
			return rubResult.Length >= 1 ? char.ToUpper(rubResult[0]) + rubResult.Substring(1) : rubResult;
		}

		public static string RusSpelledOut(
			this decimal value,
			bool male)
		{
			if (value >= 1000000000000000)
				throw new ArgumentOutOfRangeException("value");

			var str = new StringBuilder();

			if (value < 0)
			{
				str.Append("минус ");
				value = -value;
			}

			value = value
				.AppendPeriod(1000000000000, str, "триллион", "триллиона", "триллионов", true)
				.AppendPeriod(1000000000, str, "миллиард", "миллиарда", "миллиардов", true)
				.AppendPeriod(1000000, str, "миллион", "миллиона", "миллионов", true)
				.AppendPeriod(1000, str, "тысяча", "тысячи", "тысяч", false);

			var hundreds = (int)(value / 100);
			if (hundreds != 0)
				str.AppendWithSpace(_hunds[hundreds]);

			var less100 = (int)(value % 100);
			var frac20 = male ? _frac20Male : _frac20Female;
			if (less100 < 20)
				str.AppendWithSpace(frac20[less100]);
			else
			{
				var tens = less100 / 10;
				str.AppendWithSpace(_tens[tens]);
				var less10 = less100 % 10;
				if (less10 != 0)
					str.Append(" " + frac20[less100%10]);
			}

			return str.ToString();
		}

		private static void AppendWithSpace(this StringBuilder stringBuilder, string str)
		{
			if (stringBuilder.Length > 0)
				stringBuilder.Append(" ");
			stringBuilder.Append(str);
		}

		private static decimal AppendPeriod(
			this decimal value,
			long power,
			StringBuilder str,
			string declension1,
			string declension2,
			string declension5,
			bool male)
		{
			var thousands = (int)(value / power);
			if (thousands > 0)
			{
				str.AppendWithSpace(
					((decimal)thousands).RusSpelledOut(male, declension1, declension2, declension5));
				return value % power;
			}
			return value;
		}

		public static string RusSpelledOut(
			this decimal value,
			bool male,
			string valueDeclensionFor1,
			string valueDeclensionFor2,
			string valueDeclensionFor5)
		{
			return
				RusSpelledOut(value, male)
					+ " "
					+ ((int)(value % 100)).GetDeclension(valueDeclensionFor1, valueDeclensionFor2, valueDeclensionFor5);
		}

		/// <summary>
		/// Получить наименование перечислимого значения в соответствии с им самим.
		/// </summary>
		public static string GetDeclension(this int val, string one, string two, string five)
		{
			var t = (val % 100 > 20) ? val % 10 : val % 20;

			switch (t)
			{
				case 1:
					return one;
				case 2:
				case 3:
				case 4:
					return two;
				default:
					return five;
			}
		}
	}
}