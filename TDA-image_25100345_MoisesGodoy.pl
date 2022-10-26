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

pixelIsBitmap([]).
pixelIsBitmap([Pixel|Cdr]) :-
    pixbit-d(_, _, Bit, _, Pixel),
    (Bit == 0 ; Bit == 1),
    pixelIsBitmap(Cdr).

imageIsBitmap(Image) :-
    image(_, _, Pixeles, Image),
    pixelIsBitmap(Pixeles).

pixelIsPixrgb([]).
pixelIsPixrgb([Pixel|Cdr]):-
    pixrgb-d( _, _, R, G, B, _, Pixel),
    (R >= 0 , R =< 255),
    (G >= 0 , G =< 255),
    (B >= 0 , B =< 255),
    pixelIsPixrgb(Cdr).

imageIsPixmap(Image):-
    image( _, _, Pixeles, Image),
    pixelIsPixrgb(Pixeles).

pixelIsHexmap([]).
pixelIsHexmap([Pixel| Cdr]):-
    pixhex-d( _, _, Hex, _, Pixel),
    string(Hex),
    pixelIsHexmap(Cdr).

imageIsHexmap(Image) :-
    image(_, _, Pixeles, Image),
    pixelIsHexmap(Pixeles).

invierteImagenH([], _, _, ListaPixeles, ListaPixeles).
invierteImagenH([Pixel|Cdr], Ancho, Largo, NewPixeles, L):-
    pixbit-d(X, Y, Bit, Depth, Pixel),
    NewY is  (Y - (Ancho - 1)) * (-1),
    pixbit-d(X, NewY, Bit, Depth, NewPixel),
    agregar(NewPixel, NewPixeles, ListaPixeles),
    invierteImagenH(Cdr, Ancho, Largo, ListaPixeles, L).
    
invierteImagenHRGB([], _, _, ListaPixeles, ListaPixeles).
invierteImagenHRGB([Pixel|Cdr], Ancho, Largo, NewPixeles, L):-
   pixrgb-d(X, Y, R, G, B, Depth, Pixel),
   NewY is  (Y - (Ancho - 1)) * (-1),
   pixrgb-d(X, NewY, R, G, B, Depth, NewPixel),
   agregar(NewPixel, NewPixeles, ListaPixeles),
   invierteImagenHRGB(Cdr, Ancho, Largo, ListaPixeles, L).
    
flipH(I, I2):-	
    image(X, Y, Pixeles, I),
    (   pixelIsPixrgb(Pixeles)
    ->  invierteImagenHRGB(Pixeles, X, Y, _, L)
    ;   invierteImagenH(Pixeles, X, Y, _, L)
    ),
    image(X, Y, L, I2).

invierteImagenV([], _, _, ListaPixeles, ListaPixeles).
invierteImagenV([Pixel|Cdr], Ancho, Largo, NewPixeles, L):-
    pixbit-d(X, Y, Bit, Depth, Pixel),
    NewX is  (X - (Largo - 1)) * (-1),
    pixbit-d(NewX, Y, Bit, Depth, NewPixel),
    agregar(NewPixel, NewPixeles, ListaPixeles),
    invierteImagenV(Cdr, Ancho, Largo, ListaPixeles, L).

invierteImagenVRGB([], _, _, ListaPixeles, ListaPixeles).
invierteImagenVRGB([Pixel|Cdr], Ancho, Largo, NewPixeles, L):-
    pixrgb-d(X, Y, R, G, B, Depth, Pixel),
    NewX is  (X - (Largo - 1)) * (-1),
    pixrgb-d(NewX, Y, R, G, B, Depth, NewPixel),
    agregar(NewPixel, NewPixeles, ListaPixeles),
    invierteImagenVRGB(Cdr, Ancho, Largo, ListaPixeles, L).

flipV(I, I2):-
    image(X, Y, Pixeles, I),
    (   pixelIsPixrgb(Pixeles)
    ->  invierteImagenVRGB(Pixeles, X, Y, _, L)
    ;   invierteImagenV(Pixeles, X, Y, _, L)
    ),
    image(X, Y, L, I2).

crop([], _, _, _, _, _, _, ListaPixeles, ListaPixeles).
crop([Pixel|Cdr], Ancho, Largo, X1, Y1, X2, Y2, NewPixeles, L):-
    pixbit-d(X, Y, _, _, Pixel),
    (   X1 =< X , X =< X2 , Y1 =< Y , Y =< Y2
    ->  agregar(Pixel, NewPixeles, ListaPixeles)
    ;   crop(Cdr, Ancho, Largo, X1, Y1, X2, Y2, NewPixeles, L)
    ),
    crop(Cdr, Ancho, Largo, X1, Y1, X2, Y2, ListaPixeles, L).

cropRGB([], _, _, _, _, _, _, ListaPixeles, ListaPixeles).
cropRGB([Pixel|Cdr], Ancho, Largo, X1, Y1, X2, Y2, NewPixeles, L):-
    pixrgb-d(X, Y, _, _, _, _, Pixel),
    (   X1 =< X , X =< X2 , Y1 =< Y , Y =< Y2
    ->  agregar(Pixel, NewPixeles, ListaPixeles)
    ;   cropRGB(Cdr, Ancho, Largo, X1, Y1, X2, Y2, NewPixeles, L)
    ),
    cropRGB(Cdr, Ancho, Largo, X1, Y1, X2, Y2, ListaPixeles, L).

imageCrop(I, X1, Y1, X2, Y2, I2):-
    image(X, Y, Pixeles, I),
    (   pixelIsPixrgb(Pixeles)
    ->  cropRGB(Pixeles, X, Y, X1, Y1, X2, Y2, _, L)
    ;   crop(Pixeles, X, Y, X1, Y1, X2, Y2, _, L)
    ),
    NewX is ((X1 - X2) - 1)*(-1),
    NewY is ((Y1 - Y2) - 1)*(-1),
    image(NewX, NewY, L, I2).

makeHex(Num, NewCan):-
    Rest is div(Num, 16),
	M is (Num - (16 * Rest)),
    hexConvert(Rest, _, X), 
    hexConvert(M, _, Y), 
    atomic_concat(X, Y, NewCan).

hexConvert(0, 0, 0).
hexConvert(1, 1, 1).
hexConvert(2, 2, 2).
hexConvert(3, 3, 3).
hexConvert(4, 4, 4).
hexConvert(5, 5, 5).
hexConvert(6, 6, 6).
hexConvert(7, 7, 7).
hexConvert(8, 8, 8).
hexConvert(9, 9, 9).
hexConvert(10, "A", "A").
hexConvert(11, "B", "B").
hexConvert(12, "C", "C").
hexConvert(13, "D", "D").
hexConvert(14, "E", "E").
hexConvert(15, "F", "F").

rgbToHex([], _, _, ListaPixeles, ListaPixeles).
rgbToHex([Pixel|Cdr], Ancho, Largo, ListaAux, L):-
    pixrgb-d(X, Y, R, G, B, Depth, Pixel),
    makeHex(R, Cr),
    makeHex(G, Cg),
    makeHex(B, Cb),
    atomic_concat("#", Cr, Hex),
    atomic_concat(Hex, Cg, TempHex),
    atomic_concat(TempHex, Cb, AuxHex),
    pixbit-d(Y, X, AuxHex, Depth, PixelHex),
    agregar(PixelHex, ListaAux, ListaPixeles),
    rgbToHex(Cdr, Ancho, Largo, ListaPixeles, L).

imageRGBToHex(I, I2):-
    image(X, Y, Pixeles, I),
    rgbToHex(Pixeles, X, Y, _, L),
    image(X, Y, L, I2).

pixelHisto(Bit, Cant, [Bit, Cant]).

estaPixel(_, []):-!, false.
estaPixel(Pixel, [[Pixel|_]|_]):-!, true.
estaPixel(Pixel, [_|Cdr]):-
    estaPixel(Pixel, Cdr).

%pertenece( Elemento, [Elemento|_] ).
%pertenece( Elemento, [_|Resto] ) :-
	%pertenece( Elemento, Resto ).

repetidos([], _, Aux, Aux):-!.
repetidos([Pix|Cdr], Pixel, Acc, L):-
    pixbit-d( _, _, Bit, _, Pix),
    Nbit = Bit,
    (   Pixel = Nbit
    ->  Aux is Acc + 1
    ;   Aux is Acc
    ),
    repetidos(Cdr, Pixel, Aux, L).

histograma([], _, _, _, Histogram, Histogram):-!.
histograma([Pixel|Cdr], Pixeles, Ancho, Largo, ListAux, L):-
    pixbit-d( _, _, Bit, _, Pixel ),
    NewBit = Bit,
    (   estaPixel(NewBit, ListAux)
    ->  histograma(Cdr, Pixeles, Ancho, Largo, ListAux, L)
    ;   repetidos(Pixeles, NewBit, 0, Cant), agregar([NewBit,Cant], ListAux, Histogram),
        histograma(Cdr, Pixeles, Ancho, Largo, Histogram, L)
    ).

% ----- \-> PixRGB <-\----- %
estaPixelRGB(_, _, _,[]):-!, false.
estaPixelRGB(R, G, B, [[R, G, B, _]|_]):-!, true.
estaPixelRGB(R, G, B, [_|Cdr]):-
    estaPixelRGB(R, G, B, Cdr).
    
repetidosRGB([], _, Aux, Aux):-!.
repetidosRGB([Pix|Cdr], Pixel, Acc, L):-
    pixrgb-d( _, _, R, G, B, _, Pix),
    pixrgb-d( _, _, RP, GP, BP, _, Pixel),
    NewR = R, NewG = G, NewB = B,
    (   RP = NewR, GP = NewG, BP = NewB
    ->  Aux is Acc + 1
    ;   Aux is Acc
    ),
    repetidosRGB(Cdr, Pixel, Aux, L).

histogramaRGB([], _, _, _, Histogram, Histogram):-!.
histogramaRGB([Pixel|Cdr], Pixeles, Ancho, Largo, ListAux, L):-
    pixrgb-d( _, _, R, G, B, _, Pixel),
    (   estaPixelRGB(R,G,B, ListAux)
    ->  histogramaRGB(Cdr, Pixeles, Ancho, Largo, ListAux, L)
    ;   repetidosRGB(Pixeles, Pixel, 0, Cant), agregar([R,G,B, Cant], ListAux, Histogram),
        histogramaRGB(Cdr, Pixeles, Ancho, Largo, Histogram, L)
    ).
% ----- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ ----- %

imageToHistogram( I, Histograma):-
    image(X, Y, Pixeles, I),
     (   pixelIsPixrgb(Pixeles)
    ->  histogramaRGB(Pixeles, Pixeles, X, Y, _, L)
    ;   histograma(Pixeles, Pixeles, X, Y, _, L)
    ),
    image(X, Y, L, Histograma).

agregaInicio(Lista1, Elemento, [Elemento|Lista1]).

rotate90([], _, _, _, _, ImgRotada, ImgRotada).
rotate90([Pixel|Cdr], Largo, Ancho, Acum, Temp, ListAux, L):- 
    pixbit-d(_, _, Bit, Depth, Pixel),
    (   Acum = (Ancho) %agregar el pixel acutal a la lista de pixeles, porque al activar este caso se lo saltara
    ->  NewTemp is Temp - 1,  rotate90([Pixel|Cdr], Largo, Ancho, 0, NewTemp, ListAux, L)
    ;   pixbit-d(Acum, Temp, Bit, Depth, NewPixel), NewAcum is Acum + 1, agregar(NewPixel, ListAux, ImgRotada), rotate90(Cdr, Largo, Ancho, NewAcum, Temp, ImgRotada, L)
    ).

imageRotate90( I, I2):-
    image(X, Y, Pixeles, I),
    NewX is X - 1,
    rotate90(Pixeles, X, Y, 0, NewX, _, L),
    image(Y, X, L, I2).

pixelToString([], _, _, AuxL, AuxL).
pixelToString([Pixel | Pixeles], Acum, Ancho, AuxL, L):-
    pixbit-d( _, _, Bit, _, Pixel),
    NewAcum is Acum + 1,
    (   NewAcum = Ancho
    ->  atomic_concat(Bit, '\n', StrTemp), agregar(StrTemp, AuxL, ImgStr), pixelToString(Pixeles, 0, Ancho, ImgStr, L)
    ;   atomic_concat(Bit, '\t', StrTemp), agregar(StrTemp, AuxL, ImgStr), pixelToString(Pixeles, NewAcum, Ancho, ImgStr, L)
    ).

pixelToStringRGB([], _, _, AuxL, AuxL).
pixelToStringRGB([Pixel | Pixeles], Acum, Ancho, AuxL, L):-
    pixrgb-d(_, _, R, G, B, _, Pixel),
    NewAcum is Acum + 1,
    (   NewAcum = Ancho
    ->  atomic_list_concat([R,G,B], StrP), atomic_concat(StrP, '\n', StrTemp), agregar(StrTemp, AuxL, ImgStr), pixelToStringRGB(Pixeles, 0, Ancho, ImgStr, L)
    ;   atomic_list_concat([R,G,B], StrP), atomic_concat(StrP, '\t', StrTemp), agregar(StrTemp, AuxL, ImgStr), pixelToStringRGB(Pixeles, NewAcum, Ancho, ImgStr, L)
    ).

imgToString(I, ImgStr):-
    image(_, Ancho, Pixeles, I),
    (   pixelIsPixrgb(Pixeles)
    ->  pixelToStringRGB(Pixeles, 0, Ancho, _, L)
    ;   pixelToString(Pixeles, 0, Ancho, _, L)
    ),
    atomic_list_concat(L, ImgStr).

changePixel([], _, ImgMod, ImgMod).
changePixel([Pixel|Cdr], PixelMod, ListAux, L):-
    pixbit-d(Xmod, Ymod, _, _, ImgMod),
    pixbit-d(X, Y, _, _, Pixel),
    (   X = Xmod, Y = Ymod
    ->  agregar(PixelMod, ListAux, ImgMod), changePixel(Cdr, PixelMod, ImgMod, L)
    ;   changePixel(Cdr, PixelMod, ListAux, L)
    ).

changePixelRGB([], _, ImgMod, ImgMod).
changePixelRGB([Pixel|Cdr], PixelMod, ListAux, L):-
    pixrgb-d(Xmod, Ymod, _, _, _, _, PixelMod),
    pixrgb-d(X, Y, _, _, _, _, Pixel),
    (   X = Xmod, Y = Ymod
    ->  agregar(PixelMod, ListAux, ImgMod), changePixelRGB(Cdr, PixelMod, ImgMod, L)
    ;   agregar(Pixel, ListAux, ImgMod), changePixelRGB(Cdr, PixelMod, ImgMod, L)
    ).

imageInvertColorRGB(P2, P2_modificado):-
    pixrgb-d(_, _, R, G, B, _, P2),
    NewR is 255 - R, NewG is 255 - G, NewB is 255 - B,
    pixrgb-d(_, _, NewR, NewG, NewB, _, P2_modificado).
    
imageChangePixel(I, P2_modificado, I2):-
    image( X, Y, Pixeles, I),
    (   pixelIsPixrgb(Pixeles)
    ->  changePixelRGB(Pixeles, P2_modificado, _, L)
    ;   changePixel(Pixeles, P2_modificado, _, L)
    ),
    image( X, Y, L, I2).

%pixbit-d( 0, 0, 1, 10, PA), pixbit-d( 0, 1, 2, 20, PB), pixbit-d(1, 0, 3, 25, PC),
%pixbit-d( 1, 1, 4, 30, PD), pixbit-d( 2, 0, 5, 4, PE), pixbit-d(2, 1, 6, 45, PF),
%image( 3, 2, [PA, PB, PC, PD, PE, PF], I), imgToString(I, ImgStr), write(ImgStr).

% pixmap de 2X2.
% pixbit-d( 0, 0, 1, 10, PA), pixbit-d( 0, 1, 0, 20, PB), 
% pixbit-d( 1, 0, 0, 30, PC), pixbit-d( 1, 1, 1, 4, PD), 
% image( 2, 2, [PA, PB, PC, PD], I), imageCrop( I , 0, 1, 1, 1, I2 ).

% pixmap de 3X3.
% pixbit-d( 0, 0, 1, 10, PA), pixbit-d( 0, 1, 0, 20, PB), pixbit-d(0, 2, 1, 25, PC),
% pixbit-d( 1, 0, 0, 30, PD), pixbit-d( 1, 1, 1, 4, PE), pixbit-d(1, 2, 1, 45, PF),
% pixbit-d( 2, 0, 1, 50, PG), pixbit-d(2, 1, 0, 60, PH), pixbit-d(2, 2, 1, 55, PI), 
% image( 3, 3, [PA, PB, PC, PD, PE, PF, PG, PH, PI], I), imageCrop( I , 1, 2, 2, 2, I2 ).

%pixRGB-d -> rgbTohex
% pixrgb-d( 0, 0, 255, 255, 1, 10, PA), pixrgb-d( 0, 1, 0, 0, 255, 20, PB), 
% pixrgb-d( 1, 0, 0, 0, 0, 30, PC), pixrgb-d( 1, 1, 255, 255, 255, 4, PD), 
% image( 2, 2, [PA, PB, PC, PD], I), imageRGBToHex( I, I2 ).

% pixrgb-d( 0, 0, 200, 255, 101, 10, PA), pixrgb-d( 0, 1, 199, 0, 10, 20, PB), 
% pixrgb-d( 1, 0, 1, 155, 239, 30, PC), pixrgb-d( 1, 1, 123, 255, 89, 4, PD), 
% image( 2, 2, [PA, PB, PC, PD], I), imageRGBToHex( I, I2 ).

% pixbit-d( 0, 0, 1, 10, PA), pixbit-d( 0, 1, 0, 20, PB), 
% pixbit-d(1, 0, 1, 25, PC), pixbit-d( 1, 1, 0, 30, PD), 
% image( 2, 2, [PA, PB, PC, PD], I), imageToHistogram( I , Histograma).

%pixhex-d( 0, 0, '#AAFF01', 10, PA), pixhex-d( 0, 1, '#AAFF01', 20, PB), 
%pixhex-d( 1, 0, '#0001FF', 25, PC),pixhex-d( 1, 1, '#AAFF01', 30, PD), 
%image( 2, 2, [PA, PB, PC, PD], I), imageToHistogram( I , Histograma).

% pixrgb-d( 0, 0, 10, 10, 10, 10, P1), pixrgb-d( 0, 1, 20, 20, 20, 20, P2), 
% pixrgb-d( 1, 0, 30, 30, 30, 30, P3), pixrgb-d( 1, 1, 40, 40, 40, 40, P4), 
% image( 2, 2, [P1, P2, P3, P4], I), 
% pixrgb-d( 0, 1, 54, 54, 54, 20, P2_modificado), imageChangePixel(I, P2_modificado, I2).

% pixrgb-d( 0, 0, 10, 10, 10, 10, P1), pixrgb-d( 0, 1, 20, 20, 20, 20, P2), 
% pixrgb-d( 1, 0, 30, 30, 30, 30, P3), pixrgb-d( 1, 1, 40, 40, 40, 40, P4), 
% image( 2, 2, [P1, P2, P3, P4], I1), imageInvertColorRGB(P2, P2_modificado), 
% imageChangePixel(I1, P2_modificado, I2).