%TDA image mas TDA pixel.

pixbit-d(X, Y, Bit, Depth, [X, Y, Bit, Depth]).

pixhex-d(X, Y, Hex, Depth, [X, Y, Hex, Depth]).

pixrgb-d(X, Y, R, G, B, Depth, [X, Y, R, G, B, Depth]).

image(X, Y, Pixel, [X, Y, Pixel]).


%damePixeles([_, _, Car|_], Car). %funcion que extrae la lista de pixeles de una imagen.

% Si dejo de comentar la linea 15 mi invierte la lista. 
agregar(E, [], [E]).
%agregar(E, L, [E|L]).
agregar(E, [X|Y], [X|Z]):-
    agregar(E, Y, Z).

invierteImagenH([], _, _, ListaPixeles, ListaPixeles).
invierteImagenH([Pixel|Cdr], Ancho, Largo, NewPixeles, L):-
    pixbit-d(X, Y, Bit, Depth, Pixel),
    NewY is  (Y - (Ancho - 1)) * (-1),
    pixbit-d(X, NewY, Bit, Depth, NewPixel),
    agregar(NewPixel, NewPixeles, ListaPixeles),
    invierteImagenH(Cdr, Ancho, Largo, ListaPixeles, L).
    
flipH(I, I2):-	
	image(X, Y, Pixeles, I),
    invierteImagenH(Pixeles, X, Y, _, L),
    image(X, Y, L, I2).

invierteImagenV([], _, _, ListaPixeles, ListaPixeles).
invierteImagenV([Pixel|Cdr], Ancho, Largo, NewPixeles, L):-
    pixbit-d(X, Y, Bit, Depth, Pixel),
    NewX is  (X - (Largo - 1)) * (-1),
    pixbit-d(NewX, Y, Bit, Depth, NewPixel),
    agregar(NewPixel, NewPixeles, ListaPixeles),
    invierteImagenV(Cdr, Ancho, Largo, ListaPixeles, L).

flipV(I, I2):-
    image(X, Y, Pixeles, I),
    invierteImagenV(Pixeles, X, Y, _, L),
    image(X, Y, L, I2).