/*
 * The random number generator for the ants simulator.
 * 
 * As described in the problem statement.
 */

namespace Nemerle.Ants
{
  using System;

  public class AntRandom
  {
    public AntRandom (uint seed)
    {
      m_seed = seed;

      for (int i = 0; i < 4; i++)
        TickSeed ();
    }

    public uint Next ()
    {
      uint result =
        (m_seed / 65536) % 16384;

      TickSeed ();

      return result;
    }

    public uint Next (uint range)
    {
      return Next () % range;
    }

    private void TickSeed ()
    {
      unchecked
      {
        m_seed = m_seed * 22695477 + 1;
      }
    }

#if TESTING_ANT_RANDOM
    public static void Main ()
    {
      AntRandom ar = new AntRandom (12345);

      for (int i = 0; i < 100; i++)
        Console.WriteLine ("{0}", ar.Next ());
    }
#endif

    private uint m_seed;
  }
}
