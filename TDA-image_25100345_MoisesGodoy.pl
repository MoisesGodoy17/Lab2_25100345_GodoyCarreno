%TDA image mas TDA pixel.

% Meta principal: pixbit
% Meta secundaria: Null
% Dom: Int X Int X Int X Int X pixbit
% Rec: pixbit
% Descripcion: regla Constructora del TDA pixbit

pixbit-d(X, Y, Bit, Depth, [X, Y, Bit, Depth]).

% Meta principal: pixhex
% Meta secundaria: Null
% Dom: Int X Int X String X Int X pixhex
% Rec: pixhex
% Descripcion: regla Constructora del TDA pixhex

pixhex-d(X, Y, Hex, Depth, [X, Y, Hex, Depth]).

% Meta principal: pixrgb
% Meta secundaria: Null
% Dom: Int X Int X Int X Int X Int X Int X pixrgb
% Rec: pixrgb
% Descripcion: regla Constructora del TDA pirgb

pixrgb-d(X, Y, R, G, B, Depth, [X, Y, R, G, B, Depth]).

% Meta principal: image
% Meta secundaria: Null
% Dom: Int X Int X [pixrgb | pixhex| pixbit] X Image 
% Rec: Image
% Descripcion: regla Constructora del TDA image, la cual unifica los pixeles y las dimensiones de la imagen

image(X, Y, Pixel, [X, Y, Pixel]).

% Meta principal: agregar
% Meta secundaria: agregar
% Dom: [Int | String] X List X List
% Rec: pixrgb
% Descripcion: regla que agrega un elemento en una lista, para ello recibe dos listas y el elemento

agregar(E, [], [E]). % caso en que no hayan elementos en la lista
agregar(E, [X|Y], [X|Z]):- % caso recursivo donde se recorre la lista hasta llegar al caso base
    agregar(E, Y, Z).

% Meta principal: pixelIsBitmap
% Meta secundaria: pixelIsBitmap, pixbit
% Dom: List [pixrgb | pixhex| pixbit]
% Rec: Boolean
% Descripcion: regla que determina si una lista de pixeles conrresponde a una lista de pixbit

pixelIsBitmap([]).
pixelIsBitmap([Pixel|Cdr]) :-
    pixbit-d(_, _, Bit, _, Pixel),
    (Bit == 0 ; Bit == 1), % si el Bit es 0 o 1, entoces es un True y sigue con la verificacion
    pixelIsBitmap(Cdr).

% Meta principal: imageIsBitmap
% Meta secundaria: Image, pixelIsBitmap
% Dom: Image
% Rec: Boolean
% Descripcion: regla que determina si la imagen dada corresponde a un bitmap

imageIsBitmap(Image) :-
    image(_, _, Pixeles, Image),
    pixelIsBitmap(Pixeles).

% Meta principal: pixelIsPixrgb
% Meta secundaria: pixelIsPixrgb, pixrgb
% Dom: List [pixrgb | pixhex| pixbit]
% Rec: Boolean
% Descripcion: regla que determina si una lista de pixeles conrresponde a una lista de pixrgb

pixelIsPixrgb([]).
pixelIsPixrgb([Pixel|Cdr]):-
    pixrgb-d( _, _, R, G, B, _, Pixel), % en caso de entregar un pixbit o pixhex dara falso
    (R >= 0 , R =< 255), % si las componentes del RGB estan entre 0 y 255 entonces corresponden a un pixrgb
    (G >= 0 , G =< 255),
    (B >= 0 , B =< 255),
    pixelIsPixrgb(Cdr).

% Meta principal: imageIsPixmap
% Meta secundaria: Image, pixelIsPixrgb
% Dom: Image
% Rec: Boolean
% Descripcion: regla que determina si la imagen dada corresponde a un pixmap

imageIsPixmap(Image):-
    image( _, _, Pixeles, Image),
    pixelIsPixrgb(Pixeles).

% Meta principal: pixelIsHexmap
% Meta secundaria: pixelIsHexmap
% Dom: List [pixrgb | pixhex| pixbit]
% Rec: Boolean
% Descripcion: regla que determina si una lista de pixeles conrresponde a una lista de pixhex

pixelIsHexmap([]).
pixelIsHexmap([Pixel| Cdr]):-
    pixhex-d( _, _, Hex, _, Pixel),
    string(Hex), % pregunta si el bit es String
    pixelIsHexmap(Cdr).

% Meta principal: imageIsHexmap
% Meta secundaria: Image, pixelIsPixhex
% Dom: Image
% Rec: Boolean
% Descripcion: regla que determina si la imagen dada corresponde a un pixhex

imageIsHexmap(Image) :-
    image(_, _, Pixeles, Image),
    pixelIsHexmap(Pixeles).

% Meta principal: invierteImagenH
% Meta secundaria: pixbit, agregar, invierteImagenH
% Dom: List [pixbit | pixhex] X Int X Int X Symbol X Symbol
% Rec: List
% Descripcion: conjunto de reglas que invierte el valor de la coordenada "Y" de un pixel pixbit o -pixhex

invierteImagenH([], _, _, ListaPixeles, ListaPixeles).
invierteImagenH([Pixel|Cdr], Ancho, Largo, NewPixeles, L):-
    pixbit-d(X, Y, Bit, Depth, Pixel),
    NewY is  (Y - (Ancho - 1)) * (-1), % se invierte el valor de la coordenada Y
    pixbit-d(X, NewY, Bit, Depth, NewPixel), % se crea un nuevo bit con la nueva coordenada
    agregar(NewPixel, NewPixeles, ListaPixeles),
    invierteImagenH(Cdr, Ancho, Largo, ListaPixeles, L).

% Meta principal: invierteImagenHRGB
% Meta secundaria: pixrgb, agregar, invierteImagenH
% Dom: List [pixrgb] X Int X Int X Symbol X Symbol
% Rec: List
% Descripcion: conjunto de reglas que invierte el valor de la coordenada "Y" de un pixel rgb
    
invierteImagenHRGB([], _, _, ListaPixeles, ListaPixeles).
invierteImagenHRGB([Pixel|Cdr], Ancho, Largo, NewPixeles, L):-
   pixrgb-d(X, Y, R, G, B, Depth, Pixel),
   NewY is  (Y - (Ancho - 1)) * (-1),
   pixrgb-d(X, NewY, R, G, B, Depth, NewPixel), % crea el nuevo pixrgb con la coordenada nueva
   agregar(NewPixel, NewPixeles, ListaPixeles),
   invierteImagenHRGB(Cdr, Ancho, Largo, ListaPixeles, L).

% Meta principal: flipH
% Meta secundaria: Image, pixelIsPixrgb, invierteImagenHRGB, invierteImagenH
% Dom: Image X Symbol (NewImagen)
% Rec: Imagen
% Descripcion: conjunto de reglas permiten girar horizontalmente una imagen cambiando las coordenadas del eje "Y"
    
flipH(I, I2):-	
    image(X, Y, Pixeles, I),
    (   pixelIsPixrgb(Pixeles)
    ->  invierteImagenHRGB(Pixeles, X, Y, _, L)
    ;   invierteImagenH(Pixeles, X, Y, _, L)
    ),
    image(X, Y, L, I2).

% Meta principal: invierteImagenV
% Meta secundaria: pixbit, agregar, invierteImagenV
% Dom: List [pixbit | pixhex] X Int X Int X Symbol X Symbol
% Rec: List
% Descripcion: conjunto de reglas que invierte el valor de la coordenada "X" de un pixel pixbit o -pixhex

invierteImagenV([], _, _, ListaPixeles, ListaPixeles).
invierteImagenV([Pixel|Cdr], Ancho, Largo, NewPixeles, L):-
    pixbit-d(X, Y, Bit, Depth, Pixel),
    NewX is  (X - (Largo - 1)) * (-1), % invierte el bit, ej: X = 0 pase a ser X = 2 (en caso de que el largo de la imagen sea 2)
    pixbit-d(NewX, Y, Bit, Depth, NewPixel), % crea un nuevo pixel con la coordenada X invertida
    agregar(NewPixel, NewPixeles, ListaPixeles), % agrega el pixel a una lista de pixeles
    invierteImagenV(Cdr, Ancho, Largo, ListaPixeles, L).

% Meta principal: invierteImagenVRGB
% Meta secundaria: pixhex, agregar, invierteImagenVRGB
% Dom: List [pixhex] X Int X Int X Symbol X Symbol
% Rec: List
% Descripcion: conjunto de reglas que invierte el valor de la coordenada "X" de un pixel pixbit o -pixhex

invierteImagenVRGB([], _, _, ListaPixeles, ListaPixeles).
invierteImagenVRGB([Pixel|Cdr], Ancho, Largo, NewPixeles, L):-
    pixrgb-d(X, Y, R, G, B, Depth, Pixel),
    NewX is  (X - (Largo - 1)) * (-1),
    pixrgb-d(NewX, Y, R, G, B, Depth, NewPixel),
    agregar(NewPixel, NewPixeles, ListaPixeles),
    invierteImagenVRGB(Cdr, Ancho, Largo, ListaPixeles, L).

% Meta principal: flipV
% Meta secundaria: Image, pixelIsPixrgb, invierteImagenVRGB, invierteImagenV
% Dom: Image X Symbol (New Image)
% Rec: Imagen
% Descripcion: reglas que permiten girar verticalmente una imagen cambiando las coordenadas del eje "X"

flipV(I, I2):-
    image(X, Y, Pixeles, I),
    (   pixelIsPixrgb(Pixeles)
    ->  invierteImagenVRGB(Pixeles, X, Y, _, L)
    ;   invierteImagenV(Pixeles, X, Y, _, L)
    ),
    image(X, Y, L, I2).

% Meta principal: crop
% Meta secundaria: pixbit, agregar, crop
% Dom: List [pixhex | pixbit] X Int X Int X Int X Int X Int X Int X Symbol X Symbol 
% Rec: List [pixhex | pixbit]
% Descripcion: reglas que permiten recortan una imagen en un cuadrante dado, formado por 2 posiciones en la imagen

crop([], _, _, _, _, _, _, ListaPixeles, ListaPixeles).
crop([Pixel|Cdr], Ancho, Largo, X1, Y1, X2, Y2, NewPixeles, L):-
    pixbit-d(X, Y, _, _, Pixel),
    (   X1 =< X , X =< X2 , Y1 =< Y , Y =< Y2 
    ->  agregar(Pixel, NewPixeles, ListaPixeles)
    ;   crop(Cdr, Ancho, Largo, X1, Y1, X2, Y2, NewPixeles, L)
    ),
    crop(Cdr, Ancho, Largo, X1, Y1, X2, Y2, ListaPixeles, L).

% Meta principal: cropRGB
% Meta secundaria: pixrgb, agregar, cropRGB
% Dom: List [pixhex | pixbit] X Int X Int X Int X Int X Int X Int X Symbol X Symbol 
% Rec: List [pixhex | pixbit]
% Descripcion: reglas que permiten recortan una imagen contituida por pixrgb en un cuadrante dado, formado por 2 posiciones en la imagen

% si recibe una imagen [[0,0,0,10] (P1), [0,1,1,20](P2), [1,0,1,30], [1,1,0,40]] y se desea recortar la imagen entre las posiciones
% 0,1 y 1,1 recorrera la lista de pixeles preguntando si {X1 = 0 <= X(P1) = 0} = True, {X(P1) =< X2 = 0} = True, 
% {Y1 = 1 =< Y(P1) = 0} = False, { Y(P1) = 1 =< Y2 = 1} = True, como el pixel tomado no cumple todas las condiciones
% sera falso, y el siguiente (P2) si cumplira, por lo que sera agregado a la imagen recortada.

cropRGB([], _, _, _, _, _, _, ListaPixeles, ListaPixeles).
cropRGB([Pixel|Cdr], Ancho, Largo, X1, Y1, X2, Y2, NewPixeles, L):-
    pixrgb-d(X, Y, _, _, _, _, Pixel),
    (   X1 =< X , X =< X2 , Y1 =< Y , Y =< Y2
    ->  agregar(Pixel, NewPixeles, ListaPixeles) % almacena los pixeles que cumplen con la condicion de recorte
    ;   cropRGB(Cdr, Ancho, Largo, X1, Y1, X2, Y2, NewPixeles, L)
    ),
    cropRGB(Cdr, Ancho, Largo, X1, Y1, X2, Y2, ListaPixeles, L).

% Meta principal: crop
% Meta secundaria: Image, cropRGB, crop
% Dom: Image X Int X Int X Int X Int X Symbol 
% Rec: Symbol (Imagen recortada)
% Descripcion: regla que recorta los pixeles que estan dentro de las posiciones dadas como entrada

imageCrop(I, X1, Y1, X2, Y2, I2):-
    image(X, Y, Pixeles, I),
    (   pixelIsPixrgb(Pixeles)
    ->  cropRGB(Pixeles, X, Y, X1, Y1, X2, Y2, _, L)
    ;   crop(Pixeles, X, Y, X1, Y1, X2, Y2, _, L)
    ),
    NewX is ((X1 - X2) - 1)*(-1), % nuevas dimensiones de la imagen en base a las posiciones en las que se desea recortar
    NewY is ((Y1 - Y2) - 1)*(-1),
    image(NewX, NewY, L, I2).

% Meta principal: makeHes
% Meta secundaria: hexConvert
% Dom: Int X Symbol 
% Rec: Symbol (Par hexadecimal)
% Descripcion: regla recibe una componente [R | G | B] y la tranforma a Hex, para hacerlo divide el numero 
% y los restos del mismo lo tranforma a hexadecimal, retornando el numero en el formato mencionado.

makeHex(Num, NewCan):-
    Rest is div(Num, 16),
	M is (Num - (16 * Rest)),
    hexConvert(Rest, _, X),  % si el resto es igual a uno de los hechos, entonces tomara el valor (Int | String) que le corresponde al resto dado
    hexConvert(M, _, Y), 
    atomic_concat(X, Y, NewCan). % concatena el resto y el residuo del numero, unificandolo en una variable

% ----- \-> Hechos de conversion a Hexadecimal <-\----- %
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
% ----- \-> \\\\\\\\\\ <-\----- %

% Meta principal: rgbToHex
% Meta secundaria: pixrgb, makeHex, pixbit, agregar, rgbTohex
% Dom: List [pixrgb] X Int, Int X Symbol1 X Symbol2
% Rec: Symbol2 (Lista de pixeles en hexadecimal)
% Descripcion: conjunto de reglas que transforma los pixrgb a hex, tomando un pixrgb y descomponiendo sus componentes 
% para crear un numero en formato hexadecimal

rgbToHex([], _, _, ListaPixeles, ListaPixeles).
rgbToHex([Pixel|Cdr], Ancho, Largo, ListaAux, L):-
    pixrgb-d(X, Y, R, G, B, Depth, Pixel),
    makeHex(R, Cr),
    makeHex(G, Cg),
    makeHex(B, Cb),
    atomic_concat("#", Cr, Hex), % entregar la componente de R y la tranforma en Hexadecimal
    atomic_concat(Hex, Cg, TempHex),
    atomic_concat(TempHex, Cb, AuxHex),
    pixbit-d(Y, X, AuxHex, Depth, PixelHex),
    agregar(PixelHex, ListaAux, ListaPixeles), % recibe el piixel en hex y agrga a una lista
    rgbToHex(Cdr, Ancho, Largo, ListaPixeles, L).

% Meta principal: imageRGBToHex
% Meta secundaria: Image X rgbToHex 
% Dom: Image X Symbol
% Rec: Symbol (Imagen pixmap)
% Descripcion: regla que permite trasnformar una imagen pixmap a un representacion de pixhex

imageRGBToHex(I, I2):-
    image(X, Y, Pixeles, I),
    rgbToHex(Pixeles, X, Y, _, L),
    image(X, Y, L, I2).

% Meta principal: estaPixel
% Meta secundaria: estaPixel
% Dom: [Int | String] X List [pixbit | pixhex] 
% Rec: Boolean
% Descripcion: regla que determina si un pixel esta en la lista histograma, sí está retorna True, si no retorna false

estaPixel(_, []):-!, false. % si la lista es vacia, entoces no hay nada, por lo que es false, no está
estaPixel(Pixel, [[Pixel|_]|_]):-!, true. % si el elemento buscado aparece al inicio de la lista retornas true
estaPixel(Pixel, [_|Cdr]):- % caso en que no se cumple ninguna casso borde. Solo recorre la lista
    estaPixel(Pixel, Cdr).

% Meta principal: repetidos
% Meta secundaria: pixdit, repetidos
% Dom: List [pixbit | pixhex] X [Int | String] X Symbol1 X Symbol2   
% Rec: Symbol 2
% Descripcion: regla que cuenta las repeticiones de un bit dado, retornando cantidad de veces en que aparece

repetidos([], _, Aux, Aux):-!.
repetidos([Pix|Cdr], Pixel, Acc, L):-
    pixbit-d( _, _, Bit, _, Pix),
    Nbit = Bit, % si el bit actual es igual al entregado, entoces suma uno en Acc
    (   Pixel = Nbit
    ->  Aux is Acc + 1
    ;   Aux is Acc % si no, Acc se mantiene igual
    ),
    repetidos(Cdr, Pixel, Aux, L).

% Meta principal: histograma
% Meta secundaria: pixdit, estaPixel, repetidos, histograma
% Dom: List [pixbit | pixhex] X List [pixbit | pixhex] X Int X Int X Symbol1 X Symbol2
% Rec: Symbol2 (Lista formada por el histograma, donde se encuentan los pixeles y sus repeticiones)
% Descripcion: conjunto de reglas que genera una lista de los pixeles y sus repeticiones 

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

% Meta principal: estaPixelRGB
% Meta secundaria: estaPixelRGB
% Dom: Int X Int X Int X List 
% Rec: Boolean
% Descripcion: regla que determina si un pixel esta en la lista histograma, sí está retorna True, si no retorna false

estaPixelRGB(_, _, _,[]):-!, false.
estaPixelRGB(R, G, B, [[R, G, B, _]|_]):-!, true.
estaPixelRGB(R, G, B, [_|Cdr]):-
    estaPixelRGB(R, G, B, Cdr).

% Meta principal: repetidosRGB
% Meta secundaria: pixrgb, repetidosrgb
% Dom: List [pixrgb] X [Int | String] X Symbol1 X Symbol2   
% Rec: Symbol 2
% Descripcion: regla que cuenta las repeticiones de un pixrgb dado, retornando cantidad de veces en que aparece
    
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

% Meta principal: histogramaRGB
% Meta secundaria: pixrgb, estaPixelRGB, repetidosRGB, histogramaRGB
% Dom: List [pixrgb] X List [pixrgb] X Int X Int X Symbol1 X Symbol2
% Rec: Symbol2 (Lista formada por el histograma, donde se encuentan los pixeles RGB y sus repeticiones)
% Descripcion: conjunto de reglas que genera una lista de los pixeles RGB y sus repeticiones 

histogramaRGB([], _, _, _, Histogram, Histogram):-!.
histogramaRGB([Pixel|Cdr], Pixeles, Ancho, Largo, ListAux, L):-
    pixrgb-d( _, _, R, G, B, _, Pixel),
    (   estaPixelRGB(R,G,B, ListAux)
    ->  histogramaRGB(Cdr, Pixeles, Ancho, Largo, ListAux, L)
    ;   repetidosRGB(Pixeles, Pixel, 0, Cant), agregar([R,G,B, Cant], ListAux, Histogram),
        histogramaRGB(Cdr, Pixeles, Ancho, Largo, Histogram, L)
    ).
% ----- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ ----- %

% Meta principal: imageToHistogram
% Meta secundaria: image, pixelIsPixrgb, histogramaRGB, histograma
% Dom: Image X Symbol
% Rec: Symbol (Lista formada por el histograma, donde se encuentan los pixeles y sus repeticiones)
% Descripcion: regla que retorna un histograma de pixeles, compuesto por los pixeles y la cantidad de apariciones

imageToHistogram( I, Histograma):-
    image(X, Y, Pixeles, I),
    (   pixelIsPixrgb(Pixeles)
    ->  histogramaRGB(Pixeles, Pixeles, X, Y, _, Histograma)
    ;   histograma(Pixeles, Pixeles, X, Y, _, Histograma)
    ).
    %,image(X, Y, L, Histograma).

agregaInicio(Lista1, Elemento, [Elemento|Lista1]).

% Meta principal: rotate90
% Meta secundaria: pixbit, agregar, rotate90
% Dom: List [pixbit | pixhex] X List [pixbit | pixhex] X Int X Int X Int X Symbol X Symbol2 X Symbol3
% Rec: Symbol3 (lista de pixeles rotados 90 grados a la derecha)
% Descripcion: conjunto de reglas que movifican los valores X e Y de un pixel, logrando rotar la imagen 90 grados a la derecha
% Recursion: Natural

rotate90([], _, _, _, _, ImgRotada, ImgRotada).
rotate90([Pixel|Cdr], Largo, Ancho, Acum, Temp, ListAux, L):- 
    pixbit-d(_, _, Bit, Depth, Pixel),
    (   Acum = (Ancho) %agregar el pixel acutal a la lista de pixeles, porque al activar este caso se lo saltara
    ->  NewTemp is Temp - 1,  rotate90([Pixel|Cdr], Largo, Ancho, 0, NewTemp, ListAux, L)
    ;   pixbit-d(Acum, Temp, Bit, Depth, NewPixel), NewAcum is Acum + 1, agregar(NewPixel, ListAux, ImgRotada), rotate90(Cdr, Largo, Ancho, NewAcum, Temp, ImgRotada, L)
    ).

%----------\Rotate90RGB\----------%
rotate90RGB([], _, _, _, _, ImgRotada, ImgRotada).
rotate90RGB([Pixel|Cdr], Largo, Ancho, Acum, Temp, ListAux, L):- 
    pixrgb-d(_, _, R, G, B, Depth, Pixel),
    (   Acum = Ancho %agregar el pixel acutal a la lista de pixeles, porque al activar este caso se lo saltara
    ->  NewTemp is Temp - 1,  rotate90RGB([Pixel|Cdr], Largo, Ancho, 0, NewTemp, ListAux, L)
    ;   pixrgb-d(Acum, Temp, R, G, B, Depth, NewPixel), NewAcum is Acum + 1, agregar(NewPixel, ListAux, ImgRotada),
        rotate90RGB(Cdr, Largo, Ancho, NewAcum, Temp, ImgRotada, L)
    ).
%----------\\\\\\\\\\----------%

imageRotate90( I, I2):-
    image(X, Y, Pixeles, I),
    NewX is X - 1,
    (   pixelIsPixrgb(Pixeles)
    ->  rotate90RGB(Pixeles, X, Y, 0, NewX, _, L)
    ;   rotate90(Pixeles, X, Y, 0, NewX, _, L)
    ),
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

estaDepth(_, []):-!, false.
estaDepth(Depth, [Depth|_]):-!, true.
estaDepth(Depth, [_|Cdr]):-
    estaDepth(Depth, Cdr).

makeImgDepth([], _, _, ImgDepth, ImgDepth).
makeImgDepth([Pixel|Cdr], TempDepth, Depth, ListAux, L):-
    pixbit-d( _, _, _, DepthP, Pixel),
	(   Depth = DepthP
    ->  agregar(Pixel, ListAux, ImgDepth ), makeImgDepth(Cdr, TempDepth, Depth, ImgDepth, L)
    ;   agregar([TempDepth], ListAux, ImgDepth ), makeImgDepth(Cdr, TempDepth, Depth, ImgDepth, L)
    ).

makeImageDepthLayers([], _, _, _, _, ImgList, ImgList).
makeImageDepthLayers([Pixel|Cdr], CopiPixs, X, Y, RepList, ListAux, L):-
    (   pixelIsBitmap(CopiPixs)
    ->  PixelBlanco = 1
    ;   PixelBlanco = "#FFFFFF"
    ),
    pixbit-d( _, _, _, Depth, Pixel),
    (   estaDepth(Depth, RepList)
    ->  makeImageDepthLayers(Cdr, CopiPixs, X, Y, RepList, ListAux, L)
    ;   agregar(Depth, RepList, Repetidos), makeImgDepth(CopiPixs, PixelBlanco, Depth, _, ListDepth), image(X, Y, ListDepth, ImgDepth),
        agregar(ImgDepth, ListAux, ImgList), makeImageDepthLayers(Cdr, CopiPixs, X, Y, Repetidos, ImgList, L)
    ).

%----------\DepthLayersRGB\----------%
makeImgDepthRGB([], _, ImgDepth, ImgDepth).
makeImgDepthRGB([Pixel|Cdr], Depth, ListAux, L):-
    pixrgb-d( _, _, _, _, _, DepthP, Pixel),
	(   Depth = DepthP
    ->  agregar(Pixel, ListAux, ImgDepth ), makeImgDepthRGB(Cdr, Depth, ImgDepth, L)
    ;   agregar([255,255,255], ListAux, ImgDepth ), makeImgDepthRGB(Cdr, Depth, ImgDepth, L)
    ).

makeImageDepthLayersRGB([], _, _, _, _, ImgList, ImgList).
makeImageDepthLayersRGB([Pixel|Cdr], CopiPixs, X, Y, RepList, ListAux, L):-
    pixrgb-d( _, _, _, _, _, Depth, Pixel),
    (   estaDepth(Depth, RepList)
    ->  makeImageDepthLayersRGB(Cdr, CopiPixs, X, Y, RepList, ListAux, L)
    ;   agregar(Depth, RepList, Repetidos), makeImgDepthRGB(CopiPixs, Depth, _, ListDepth), image(X, Y, ListDepth, ImgDepth),
        agregar(ImgDepth, ListAux, ImgList), makeImageDepthLayersRGB(Cdr, CopiPixs, X, Y, Repetidos, ImgList, L)
    ).
%----------\\\\\\\\\\----------%

imageDepthLayers(I, LI):-
    image( X, Y, Pixeles, I),
    (   pixelIsPixrgb(Pixeles)
    ->  makeImageDepthLayersRGB(Pixeles, Pixeles, X, Y, _, _, LI)
    ;   makeImageDepthLayers(Pixeles, Pixeles, X, Y, _, _, LI)
    ).

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

% pixhex-d( 0, 0, '#AAFF01', 10, PA), pixhex-d( 0, 1, '#AAFF01', 20, PB), 
% pixhex-d( 1, 0, '#0001FF', 25, PC),pixhex-d( 1, 1, '#AAFF01', 30, PD), 
% image( 2, 2, [PA, PB, PC, PD], I), imageToHistogram( I , Histograma).

% pixrgb-d( 0, 0, 10, 10, 10, 10, P1), pixrgb-d( 0, 1, 20, 20, 20, 20, P2), 
% pixrgb-d( 1, 0, 30, 30, 30, 30, P3), pixrgb-d( 1, 1, 40, 40, 40, 40, P4), 
% image( 2, 2, [P1, P2, P3, P4], I), imageRotate90(I, I2).

% pixrgb-d( 0, 0, 10, 10, 10, 10, P1), pixrgb-d( 0, 1, 20, 20, 20, 20, P2), 
% pixrgb-d( 1, 0, 30, 30, 30, 30, P3), pixrgb-d( 1, 1, 40, 40, 40, 40, P4), 
% image( 2, 2, [P1, P2, P3, P4], I), 
% pixrgb-d( 0, 1, 54, 54, 54, 20, P2_modificado), imageChangePixel(I, P2_modificado, I2).

% pixrgb-d( 0, 0, 10, 10, 10, 10, P1), pixrgb-d( 0, 1, 20, 20, 20, 20, P2), 
% pixrgb-d( 1, 0, 30, 30, 30, 30, P3), pixrgb-d( 1, 1, 40, 40, 40, 40, P4), 
% image( 2, 2, [P1, P2, P3, P4], I1), imageInvertColorRGB(P2, P2_modificado), 
% imageChangePixel(I1, P2_modificado, I2).

% pixrgb-d( 0, 0, 10, 10, 10, 10, P1), pixrgb-d( 0, 1, 20, 20, 20, 10, P2), 
% pixrgb-d( 1, 0, 30, 30, 30, 30, P3), pixrgb-d( 1, 1, 40, 40, 40, 40, P4), 
% image( 2, 2, [P1, P2, P3, P4], I), imageDepthLayers(I, LI).
