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

crop([], _, _, _, _, _, _, ListaPixeles, ListaPixeles).
crop([Pixel|Cdr], Ancho, Largo, X1, Y1, X2, Y2, NewPixeles, L):-
    pixbit-d(X, Y, _, _, Pixel),
    (   X1 =< X , X =< X2 , Y1 =< Y , Y =< Y2
    ->  agregar(Pixel, NewPixeles, ListaPixeles)
    ;   crop(Cdr, Ancho, Largo, X1, Y1, X2, Y2, NewPixeles, L)
    ),
    crop(Cdr, Ancho, Largo, X1, Y1, X2, Y2, ListaPixeles, L).

imageCrop(I, X1, Y1, X2, Y2, I2):-
    image(X, Y, Pixeles, I),
    crop(Pixeles, X, Y, X1, Y1, X2, Y2, _, L),
    NewX is ((X1 - X2) - 1)*(-1),
    NewY is ((Y1 - Y2) - 1)*(-1),
    image(NewX, NewY, L, I2).

% pixmap de 2X2.
% pixbit-d( 0, 0, 1, 10, PA), pixbit-d( 0, 1, 0, 20, PB), 
% pixbit-d( 1, 0, 0, 30, PC), pixbit-d( 1, 1, 1, 4, PD), 
% image( 2, 2, [PA, PB, PC, PD], I), imageCrop( I , 0, 1, 1, 1, I2 ).

% pixmap de 3X3.
% pixbit-d( 0, 0, 1, 10, PA), pixbit-d( 0, 1, 0, 20, PB), pixbit-d(0, 2, 1, 25, PC),
% pixbit-d( 1, 0, 0, 30, PD), pixbit-d( 1, 1, 1, 4, PE), pixbit-d(1, 2, 1, 45, PF),
% pixbit-d( 2, 0, 1, 50, PG), pixbit-d(2, 1, 0, 60, PH), pixbit-d(2, 2, 1, 55, PI), 
% image( 3, 3, [PA, PB, PC, PD, PE, PF, PG, PH, PI], I), imageCrop( I , 1, 2, 2, 2, I2 ).

makeHex(Num, ListRest, L):-
    Rest is div(Num, 16),
	M is (Num - (16 * Rest)),
    agregar(Rest, L, ListRest),
    agregar(M, L, ListRest).


rgbToHex([], _, _, ListaPixeles, ListaPixeles).
rgbToHex([Pixel|Cdr], Ancho, Largo, _, ListaAux):-
    pixrgb-d(_, _, R, G, B, _, Pixel),
    makeHex(R, ListaPixeles, L),
    makeHex(G, ListaPixeles, L),
    makeHex(B, ListaPixeles, L),
    %agregar(Pixel, ListaAux, ListaPixeles),
    rgbToHex(Cdr, Ancho, Largo, ListaPixeles, ListaAux).

imageRGBToHex(I, I2):-
    image(X, Y, Pixeles, I),
    rgbToHex(Pixeles, X, Y, _, L),
    image(X, Y, L, I2).


%pixRGB-d -> rgbTohex
% pixrgb-d( 0, 0, 255, 255, 1, 10, PA), pixrgb-d( 0, 1, 0, 0, 255, 20, PB), 
% pixrgb-d( 1, 0, 0, 0, 0, 30, PC), pixrgb-d( 1, 1, 255, 255, 255, 4, PD), 
% image( 2, 2, [PA, PB, PC, PD], I), imageRGBToHex( I, I2 ).

%N is div(255, 16),
	% M is (255 - (16 * N)),
	% (   N = 12
	% ->  Acc = "C"
	% ;   Acc = N
	% ).