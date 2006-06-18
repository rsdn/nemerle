Era SMS Sender

Program umozliwia wysylanie SMSow wylacznie do sieci Era, wylacznie poprzez konto
mailowe. Po uruchomieniu program skanuje wszystkie podane skrzynki pocztowe i sprawdza
czy nie znajduje sie w ktorejs wiadomosc z aktualnym ('dzisiejszym') haslem. Haslo
znajdzie sie na skrzynce jesli zostanie podjeta proba zalogowania sie w serwisie WWW.
Jesli taka proba zostala podjeta, Era przysyla haslo w formie zalacznika, w ktorym jest
umieszczony obrazek GIF zakodowany w base64. Program sciaga tylko te wiadomosci, ktore
zawieraja haslo.


Uzytkownik w panelu z kontami moze wykonac nastepujace czynnosci:
- Dodanie nowego konta (login na serwer, nazwa serwera oraz haslo)
- Usuniecie istniejacych kont
- Zmiane istniejacego konta
- Rozpoznanie hasel sciagnietych z konta
- Ponowne przeskanowanie skrzynki
- Zamowienie hasla (proba logowanie sie w serwisie WWW Ery)


Dodawanie nowego uzytkownika:

Po wybraniu tej opcji uzytkownik jest proszony o podanie loginu na serwerze, nazwy serwera
oraz hasla do konta. Uwaga: serwerem jest serwer POP3.


Usuniecie istniejacych kont:

Po wybraniu tej opcji zostana usuniete wszystkie zaznaczone konta.


Zmiana istniejacego konta:

Po wybraniu tej opcji uzytkownik moze zmienic wpisane wczesniej dane.


Rozpoznawanie hasel sciagnietych z konta:

Po wybraniu tej opcji uzytkownik powinien rozpoznac hasla. To znaczy zamienic je
z wyswietlonej reprezentacji graficznej na reprezentacje tekstowa. Nastepnie sa sprawdzane
wszystkie rozpoznane hasla.


Zamawianie hasel:

Jesli nazwa konta + @ + nazwa serwera tworza adres mail zarejestrowany w sieci Era opcja ta
umozliwia automatyczne zamawianie hasel.


Program skladuje informacje o kontach w pliku XML. Plik jest czytany przy uruchomieniu
aplikacji. Zapisywany - przy zamykaniu aplikacji. Hasla sa trzymane w postaci jawnej.


Uwagi:
- Program dziala napewno pod Windows 2000/XP z .NET Framework v1.1.
- Czasami moga wyskakiwac dane debugujace. (nie wiem czy wszystkie usunalem)