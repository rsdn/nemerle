NAME    = gon
SRC     = Game.n Goban.n GtkGame.n Main.n GnuGoPlayer.n
MONO    = mono
COMPILE = ncc

all: $(SRC)
	$(COMPILE) -pkg gtk-sharp -texe -out:$(NAME).exe $(SRC)

doc: $(SRC)
	$(COMPILE) -pkg gtk-sharp -texe -out:$(NAME).exe $(SRC) -doc:doc/gon.xml
run: all
	$(MONO) $(NAME).exe mono 9 

clean:
	rm -f $(NAME).exe
