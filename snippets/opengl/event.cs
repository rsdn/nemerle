using Tao.Sdl;

public class EventProducer {
  static public Sdl.SDL_Event Get () {
    Sdl.SDL_Event e;
    Sdl.SDL_PollEvent (out e);
    return e;
  }
}
