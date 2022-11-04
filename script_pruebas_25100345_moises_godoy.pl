:- include(tda_pixbit_25100345_moises_godoy).
:- include(tda_pixrgb_25100345_moises_godoy).
:- include(tda_pixhex_25100345_moises_godoy).
:- include(tda_image_25100345_moises_godoy).


% -> Probar que se puede generar una imagen pixbit

% pixbit( 0, 0, 1, 10, PA), pixbit( 0, 1, 0, 20, PB), 
% pixbit( 1, 0, 0, 30, PC), pixbit( 1, 1, 1, 4, PD), 
% image( 2, 2, [PA, PB, PC, PD], I), 
% imageIsBitmap(I), imageToString(I, Str), write(Str).

%----////----%
% pixbit( 0, 0, 0, 10, PA), pixbit( 0, 1, 1, 20, PB),
% pixbit(1, 0, 1, 25, PC), pixbit( 1, 1, 1, 30, PD),
% image( 2, 2, [PA, PB, PC, PD], I), imgToString(I, ImgStr), write(ImgStr).

% pixbit( 0, 0, 1, 10, PA), pixbit( 0, 1, 1, 20, PB), pixbit(1, 0, 1, 25, PC),
% pixbit( 1, 1, 0, 30, PD), pixbit( 2, 0, 0, 4, PE), pixbit(2, 1, 0, 45, PF),
% image( 3, 2, [PA, PB, PC, PD, PE, PF], I), imgToString(I, ImgStr), write(ImgStr).

% pixbit( 0, 0, 1, 10, PA), pixbit( 0, 1, 0, 20, PB), pixbit(1, 0, 0, 25, PC),
% pixbit( 1, 1, 0, 30, PD), pixbit( 2, 0, 1, 4, PE), pixbit(2, 1, 1, 45, PF),
% pixbit( 1, 1, 0, 30, PG), pixbit( 2, 0, 1, 4, PH), pixbit(2, 1, 0, 45, PI),
% image( 3, 3, [PA, PB, PC, PD, PE, PF, PG, PH, PI], I), imgToString(I, ImgStr), write(ImgStr).
%----////----%

% -> Probar que imageIsBitMap detecta cuando se tiene una imagen en hex o en rgb.
% pixbit( 0, 0, 1, 10, PA), pixbit( 0, 1, 0, 20, PB), 
% pixbit( 1, 0, 0, 30, PC), pixbit( 1, 1, 1, 4, PD), 
% image( 2, 2, [PA, PB, PC, PD], I), imageIsBitmap( I ).

% Estos casos deben dar false:
% pixhex( 0, 0, "#FF0000", 10, PA), pixhex( 0, 1, "#FF0000", 20, PB), 
% pixhex( 1, 0, "#0000FF", 30, PC), pixhex( 1, 1, "#0000FF", 4, PD), 
% image( 2, 2, [PA, PB, PC, PD], I), imageIsBitmap( I ).

% pixrgb( 0, 0, 200, 200, 200, 10, PA), pixrgb( 0, 1, 200, 200, 200, 20, PB), 
% pixrgb( 1, 0, 190, 190, 190, 30, PC), pixrgb( 1, 1, 190, 190, 190, 4, PD), 
% image( 2, 2, [PA, PB, PC, PD], I), imageIsBitmap( I ).

%----////----%
% pixrgb( 0, 0, 255, 255, 1, 10, PA), pixrgb( 0, 1, 0, 0, 255, 20, PB),
% pixrgb( 1, 0, 0, 0, 0, 30, PC), pixrgb( 1, 1, 255, 255, 255, 4, PD),
% image( 2, 2, [PA, PB, PC, PD], I), imageIsBitmap( I ).

% pixhex( 0, 0, "#AAFF01", 10, PA), pixhex( 0, 1, "#AAFF01", 20, PB),
% pixhex( 1, 0, "#0001FF", 25, PC), pixhex( 1, 1, "#AAFF01", 30, PD),
% image( 2, 2, [PA, PB, PC, PD], I), imageIsBitmap( I ).

% pixbit( 0, 0, 1, 10, PA), pixbit( 0, 1, 1, 20, PB),
% pixbit(1, 0, 0, 25, PC), pixbit( 1, 1, "#FF00AB", 30, PD),
% image( 2, 2, [PA, PB, PC, PD], I), imageIsBitmap( I ).
%----////----%

% -> Probar que se puede generar una imagen pixhex
% pixhex( 0, 0, "#FF0000", 10, PA), pixhex( 0, 1, "#FF0000", 20, PB), 
% pixhex( 1, 0, "#0000FF", 30, PC), pixhex( 1, 1, "#0000FF", 4, PD), 
% image( 2, 2, [PA, PB, PC, PD], I), imageToString(I, Str), write(Str).

%----////----%
% pixhex( 0, 0, "#001100", 10, PA), pixhex( 0, 1, "#FFFFFF", 20, PB), pixhex(1, 0, "#090909", 25, PC),
% pixhex( 1, 1, "#FF0000", 4, PD), pixhex( 2, 0, "#AABBCC", 4, PE), pixhex(2, 1, "#FF1111", 45, PF),
% pixhex( 1, 1, "#111111", 30, PG), pixhex( 2, 0, "FF00FF", 4, PH), pixhex(2, 1, "00FF00", 45, PI),
% image( 3, 3, [PA, PB, PC, PD, PE, PF, PG, PH, PI], I), imageToString(I, Str), write(Str).

% pixhex( 0, 0, "#AAFF01", 10, PA), pixhex( 0, 1, "#AAFF01", 20, PB),
% pixhex( 1, 0, "#0001FF", 25, PC), pixhex( 1, 1, "#AAFF01", 30, PD),
% image( 2, 2, [PA, PB, PC, PD], I), imageToString(I, Str), write(Str).

% pixhex( 0, 0, "#001100", 10, PA), pixhex( 0, 1, "#111111", 20, PB), pixhex(1, 0, "#FFFFFF", 25, PC),
% pixhex( 1, 1, "#AAAAFF", 30, PD), pixhex( 2, 0, "#11FFFF", 4, PE), pixhex(2, 1, "#8CB8FF", 45, PF),
% image( 3, 2, [PA, PB, PC, PD, PE, PF], I), imgToString(I, ImgStr), write(ImgStr).
%----////----%

% -> Probar que imageIsHexmap detecta cuando se tiene una imagen en bit o en rgb.
% pixhex( 0, 0, "#FF0000", 10, PA), pixhex( 0, 1, "#FF0000", 20, PB), 
% pixhex( 1, 0, "#0000FF", 30, PC), pixhex( 1, 1, "#0000FF", 4, PD), 
% image( 2, 2, [PA, PB, PC, PD], I), imageIsHexmap( I ).

% Estos casos deben dar false:
% pixbit( 0, 0, 1, 10, PA), pixbit( 0, 1, 0, 20, PB),
% pixbit( 1, 0, 0, 30, PC), pixbit( 1, 1, 1, 4, PD), 
% image( 2, 2, [PA, PB, PC, PD], I), imageIsHexmap( I ).

% pixrgb( 0, 0, 200, 200, 200, 10, PA), pixrgb( 0, 1, 200, 200, 200, 20, PB), 
% pixrgb( 1, 0, 190, 190, 190, 30, PC), pixrgb( 1, 1, 190, 190, 190, 4, PD), 
% image( 2, 2, [PA, PB, PC, PD], I), imageIsHexmap( I ).

%----////----%
% pixhex( 0, 0, "#001100", 10, PA), pixhex( 0, 1, 1, 20, PB), pixhex(1, 0, "#090909", 25, PC),
% pixhex( 1, 1, "#FF0000", 4, PD), pixhex( 2, 0, 1, 4, PE), pixhex(2, 1, "#FF1111", 45, PF),
% pixhex( 1, 1, "#111111", 30, PG), pixhex( 2, 0, 0, 4, PH), pixhex(2, 1, "#00FF00", 45, PI),
% image( 3, 3, [PA, PB, PC, PD, PE, PF, PG, PH, PI], I), imageToString(I, Str), write(Str).

% pixrgb( 0, 0, 255, 255, 1, 10, PA), pixrgb( 0, 1, 0, 0, 255, 20, PB),
% pixrgb( 1, 0, 0, 0, 0, 30, PC), pixrgb( 1, 1, 255, 255, 255, 4, PD),
% image( 2, 2, [PA, PB, PC, PD], I), imageIsBitmap( I ).

% pixbit( 0, 0, 1, 10, PA), pixbit( 0, 1, 0, 20, PB),
% pixbit(1, 0, 0, 25, PC), pixbit( 1, 1, 1, 30, PD),
% image( 2, 2, [PA, PB, PC, PD], I), imageIsBitmap( I ).
%----////----%

% -> Probar que se puede generar una imagen pixrgb
% pixrgb( 0, 0, 255, 0, 0, 10, PA), pixrgb( 0, 1, 255, 0, 0, 20, PB),
% pixrgb( 1, 0, 0, 0, 255, 30, PC), pixrgb( 1, 1, 0, 0, 255, 4, PD), 
% image( 2, 2, [PA, PB, PC, PD], I), imageToString(I, Str), write(Str).

%----////----%
% pixrgb( 0, 0, 255, 255, 1, 10, PA), pixrgb( 0, 1, 0, 0, 255, 20, PB),
% pixrgb( 1, 0, 0, 0, 0, 30, PC), pixrgb( 1, 1, 255, 255, 255, 4, PD),
% pixrgb( 2, 0, 1, 1, 1, 30, PE), pixrgb( 2, 1, 255, 1, 255, 4, PF),
% image( 3, 2, [PA, PB, PC, PD, PE, PF], I), imageToString(I, Str), write(Str).

% pixrgb( 0, 0, 255, 255, 1, 10, PA), pixrgb( 0, 1, 0, 0, 255, 20, PB),
% pixrgb( 1, 0, 0, 0, 0, 30, PC), pixrgb( 1, 1, 255, 255, 255, 4, PD),
% image( 2, 2, [PA, PB, PC, PD], I), imageToString(I, Str), write(Str).

% pixrgb( 0, 0, 10, 10, 10, 10, P1), pixrgb( 0, 1, 20, 20, 20, 20, P2),
% pixrgb( 1, 0, 30, 30, 30, 30, P3), pixrgb( 1, 1, 40, 40, 40, 40, P4),
% image( 2, 2, [P1, P2, P3, P4], I), imageToString(I, Str), write(Str).
%----////----%

% -> Probar que imageIsPixmap detecta cuando se tiene una imagen en hex o en bit.
% pixrgb( 0, 0, 200, 200, 200, 10, PA), pixrgb( 0, 1, 200, 200, 200, 20, PB), 
% pixrgb( 1, 0, 190, 190,190, 30, PC), pixrgb( 1, 1, 190, 190, 190, 4, PD), 
% image( 2, 2, [PA, PB, PC, PD], I), imageIsPixmap( I ).

% Estos casos deben dar false:
% pixbit( 0, 0, 1, 10, PA), pixbit( 0, 1, 0, 20, PB), 
% pixbit( 1, 0, 0, 30, PC), pixbit( 1, 1, 1, 4, PD), 
% image( 2, 2, [PA, PB, PC, PD], I), imageIsPixmap( I ).

% pixhex( 0, 0, "#FF0000", 10, PA), pixhex( 0, 1, "#FF0000", 20, PB), 
% pixhex( 1, 0, "#0000FF", 30, PC), pixhex( 1, 1, "#0000FF", 4, PD), 
% image( 2, 2, [PA, PB, PC, PD], I), imageIsPixmap( I ).

%----////----%
% pixhex( 0, 0, "#001100", 10, PA), pixhex( 0, 1, 1, 20, PB), pixhex(1, 0, "#090909", 25, PC),
% pixhex( 1, 1, "#FF0000", 4, PD), pixhex( 2, 0, "#FF00FF", 4, PE), pixhex(2, 1, "#FF1111", 45, PF),
% pixhex( 1, 1, "#111111", 30, PG), pixhex( 2, 0, "11FF00", 4, PH), pixhex(2, 1, "00FF00", 45, PI),
% image( 3, 3, [PA, PB, PC, PD, PE, PF, PG, PH, PI], I), imageIsPixmap( I ).

% pixrgb( 0, 0, 1, 0, 0, 10, PA), pixrgb( 0, 1, 0, 0, 255, 20, PB),
% pixrgb( 1, 0, 250, 255, 0, 30, PC), pixrgb( 1, 1, 255, 255, 255, 4, PD),
% image( 2, 2, [PA, PB, PC, PD], I), imageIsPixmap( I ).

% pixbit( 0, 0, 0, 10, PA), pixbit( 0, 1, 0, 20, PB),
% pixbit(1, 0, 1, 25, PC), pixbit( 1, 1, 1, 30, PD),
% image( 2, 2, [PA, PB, PC, PD], I), imageIsPixmap( I ).
%----////----%

% -> Convierte una imagen RGB a HEX y comprueba con los predicados de pertenencia, 
% luego convierte a string y muestra por pantalla:
% 
% pixrgb( 0, 0, 200, 200, 200, 10, PA), pixrgb( 0, 1, 200, 200, 200, 20, PB), 
% pixrgb( 1, 0, 190, 190,190, 30, PC), pixrgb( 1, 1, 190, 190, 190, 4, PD), 
% image( 2, 2, [PA, PB, PC, PD], I), imageIsPixmap( I ), imageRGBToHex(I, I2), 
% imageIsHexmap(I2), imageToString(I2, Str), write(Str).

%----////----%
% pixrgb( 0, 0, 1, 0, 0, 10, PA), pixrgb( 0, 1, 0, 0, 255, 20, PB),
% pixrgb( 1, 0, 1, 255, 0, 30, PC), pixrgb( 1, 1, 255, 255, 255, 4, PD),
% image( 2, 2, [PA, PB, PC, PD], I), imageIsPixmap( I ), imageRGBToHex(I, I2), 
% imageIsHexmap(I2), imageToString(I2, Str), write(Str).

% pixrgb( 0, 0, 200, 200, 200, 10, PA), pixrgb( 0, 1, 200, 200, 200, 20, PB), 
% pixrgb( 1, 0, 190, 190, 190, 30, PC), pixrgb( 1, 1, 190, 190, 190, 4, PD), 
% image( 2, 2, [PA, PB, PC, PD], I), imageIsPixmap( I ), imageRGBToHex(I, I2), 
% imageIsHexmap(I2), imageToString(I2, Str), write(Str).

% pixrgb( 0, 0, 1, 0, 0, 10, PA), pixrgb( 0, 1, 0, 0, 255, 20, PB),
% pixrgb( 1, 0, 255, 255, 0, 30, PC), pixrgb( 1, 1, 255, 255, 255, 4, PD),
% pixrgb( 2, 0, 255, 0, 0, 30, PE), pixrgb( 2, 1, 1, 1, 80, 4, PF),
% image( 3, 2, [PA, PB, PC, PD, PE, PF], I), imageIsPixmap( I ), imageRGBToHex(I, I2), 
% imageIsHexmap(I2), imageToString(I2, Str), write(Str).
%----////----%

% -> Comprime una imagen, luego descomprime y debe resultar la misma imagen original:
%
% pixbit( 0, 0, 1, 10, PA), pixbit( 0, 1, 0, 20, PB), 
% pixbit( 1, 0, 0, 30, PC), pixbit( 1, 1, 1, 4, PD), 
% image( 2, 2, [PA, PB, PC, PD], I), imageCompress(I, I2), imageDecompress(I2, I3).
% En el ejemplo anterior “I” debería ser igual a “I3”

% -> Si se rota una imagen 4 veces en 90°, debería resultar la imagen original:
% 
% pixhex( 0, 0, "#FF0000", 10, PA), pixhex( 0, 1, "#FF0000", 20, PB), 
% pixhex( 1, 0, "#0000FF", 30, PC), pixhex( 1, 1, "#0000FF", 4, PD), 
% image( 2, 2, [PA, PB, PC, PD], I), imageRotate90(I, I2), 
% imageRotate90(I2, I3), imageRotate90(I3, I4), imageRotate90(I4, I5).
% En el ejemplo anterior “I” debería ser igual a “I5”

%----////----%
% pixhex( 0, 0, "#001100", 10, PA), pixhex( 0, 1, "#000000", 20, PB), pixhex(1, 0, "#090909", 25, PC),
% pixhex( 1, 1, "#FF0000", 4, PD), pixhex( 2, 0, "#FFFF11", 4, PE), pixhex(2, 1, "#FF1111", 45, PF),
% pixhex( 1, 1, "#111111", 30, PG), pixhex( 2, 0, "#AAFF11", 4, PH), pixhex(2, 1, "#00FF00", 45, PI),
% image( 3, 3, [PA, PB, PC, PD, PE, PF, PG, PH, PI], I), imageRotate90(I, I2), 
% imageRotate90(I2, I3), imageRotate90(I3, I4), imageRotate90(I4, I5).

% pixrgb( 0, 0, 255, 255, 1, 10, PA), pixrgb( 0, 1, 0, 0, 255, 20, PB),
% pixrgb( 1, 0, 0, 0, 0, 30, PC), pixrgb( 1, 1, 255, 255, 255, 4, PD),
% image( 2, 2, [PA, PB, PC, PD], I), imageRotate90(I, I2), 
% imageRotate90(I2, I3), imageRotate90(I3, I4), imageRotate90(I4, I5).

% pixbit( 0, 0, 1, 10, PA), pixbit( 0, 1, 0, 20, PB),
% pixbit(1, 0, 1, 25, PC), pixbit( 1, 1, 1, 30, PD),
% image( 2, 2, [PA, PB, PC, PD], I), imageRotate90(I, I2), 
% imageRotate90(I2, I3), imageRotate90(I3, I4), imageRotate90(I4, I5).
%----////----%

% -> Si se rota una imagen en 90° que tiene el mismo color y profundidad en 
% todos sus píxeles, entonces la imagen resultante debería ser la misma imagen original.
% 
% pixhex( 0, 0, "#FF0000", 30, PA), pixhex( 0, 1, "#FF0000", 30, PB), 
% pixhex( 1, 0, "#FF0000", 30, PC), pixhex( 1, 1, "#FF0000", 30, PD), 
% image( 2, 2, [PA, PB, PC, PD], I), imageRotate90(I, I2).
% En el ejemplo anterior “I” debería ser igual a “I2”

%----////----%
% pixrgb( 0, 0, 255, 255, 255, 10, PA), pixrgb( 0, 1, 255, 255, 255, 10, PB),
% pixrgb( 1, 0, 255, 255, 255, 10, PC), pixrgb( 1, 1, 255, 255, 255, 10, PD),
% image( 2, 2, [PA, PB, PC, PD], I), imageRotate90(I, I2).

% pixhex( 0, 0, "#FFFFFF", 20, PA), pixhex( 0, 1, "#FFFFFF", 20, PB), pixhex(1, 0, "#FFFFFF", 20, PC),
% pixhex( 1, 1, "#FFFFFF", 20, PD), pixhex( 2, 0, "#FFFFFF", 20, PE), pixhex(2, 1, "#FFFFFF", 20, PF),
% image( 3, 2, [PA, PB, PC, PD, PE, PF], I), imageRotate90(I, I2).

% pixbit( 0, 0, 1, 10, PA), pixbit( 0, 1, 1, 10, PB), 
% pixbit( 1, 0, 1, 10, PC), pixbit( 1, 1, 1, 10, PD), 
% image( 2, 2, [PA, PB, PC, PD], I), imageRotate90(I, I2).
%----////----%

% -> Si se hace imageFlipV dos veces de una imagen, debería resultar la imagen original:
% 
% pixhex( 0, 0, "#FF0000", 10, PA), pixhex( 0, 1, "#FF0000", 20, PB), 
% pixhex( 1, 0, "#0000FF", 30, PC), pixhex( 1, 1, "#0000FF", 4, PD), 
% image( 2, 2, [PA, PB, PC, PD], I), imageFlipV(I, I2), imageFlipV(I2, I3).
% En el ejemplo anterior “I” debería ser igual a “I3”

%----////----%
% pixhex( 0, 0, "#000000", 10, PA), pixhex( 0, 1, "#FF00FF", 20, PB), pixhex(1, 0, "#090909", 25, PC),
% pixhex( 1, 1, "#FF0000", 4, PD), pixhex( 2, 0, "#00FF01", 4, PE), pixhex(2, 1, "#FF00AA", 45, PF),
% image( 3, 2, [PA, PB, PC, PD, PE, PF, PG, PH, PI], I), imageFlipV(I, I2), 
% imageFlipV(I2, I3).

% pixrgb( 0, 0, 255, 255, 1, 10, PA), pixrgb( 0, 1, 0, 0, 255, 20, PB),
% pixrgb( 1, 0, 0, 0, 0, 30, PC), pixrgb( 1, 1, 255, 255, 255, 4, PD),
% image( 2, 2, [PA, PB, PC, PD], I), imageFlipV(I, I2), 
% imageFlipV(I2, I3).

% pixbit( 0, 0, 1, 10, PA), pixbit( 0, 1, 0, 20, PB),
% pixbit(1, 0, 1, 25, PC), pixbit( 1, 1, 1, 30, PD),
% image( 2, 2, [PA, PB, PC, PD], I), imageFlipV(I, I2), 
% imageFlipV(I2, I3).
%----////----%

% -> Si se hace imageFlipH dos veces de una imagen, debería resultar la imagen original:
% 
% pixhex( 0, 0, "#FF0000", 10, PA), pixhex( 0, 1, "#FF0000", 20, PB), 
% pixhex( 1, 0, "#0000FF", 30, PC), pixhex( 1, 1, "#0000FF", 4, PD), 
% image( 2, 2, [PA, PB, PC, PD], I), imageFlipH(I, I2), imageFlipH(I2, I3).
% En el ejemplo anterior “I” debería ser igual a “I3”

%----////----%
% pixhex( 0, 0, "#000000", 10, PA), pixhex( 0, 1, "#FF00FF", 20, PB), pixhex(1, 0, "#090909", 25, PC),
% pixhex( 1, 1, "#FF0000", 4, PD), pixhex( 2, 0, "#00FF01", 4, PE), pixhex(2, 1, "#FF00AA", 45, PF),
% image( 3, 2, [PA, PB, PC, PD, PE, PF, PG, PH, PI], I), imageFlipH(I, I2), 
% imageFlipH(I2, I3).

% pixrgb( 0, 0, 255, 255, 1, 10, PA), pixrgb( 0, 1, 0, 0, 255, 20, PB),
% pixrgb( 1, 0, 0, 0, 0, 30, PC), pixrgb( 1, 1, 255, 255, 255, 4, PD),
% image( 2, 2, [PA, PB, PC, PD], I), imageFlipH(I, I2), 
% imageFlipH(I2, I3).
%
% pixbit( 0, 0, 1, 10, PA), pixbit( 0, 1, 0, 20, PB),
% pixbit(1, 0, 1, 25, PC), pixbit( 1, 1, 1, 30, PD),
% image( 2, 2, [PA, PB, PC, PD], I), imageFlipH(I, I2), 
% imageFlipH(I2, I3).
%----////----%

% -> Si se hace imageFlipH a una imagen que tiene el mismo color y profundidad en 
% todos sus pixeles, entonces la imagen resultante debería ser la misma imagen original.
% 
% pixhex( 0, 0, "#FF0000", 30, PA), pixhex( 0, 1, "#FF0000", 30, PB), 
% pixhex( 1, 0, "#FF0000", 30, PC), pixhex( 1, 1, "#FF0000", 30, PD), 
% image( 2, 2, [PA, PB, PC, PD], I), imageFlipH(I, I2).
% En el ejemplo anterior “I” debería ser igual a “I2”

%----////----%
% pixrgb( 0, 0, 255, 255, 255, 10, PA), pixrgb( 0, 1, 255, 255, 255, 10, PB),
% pixrgb( 1, 0, 255, 255, 255, 10, PC), pixrgb( 1, 1, 255, 255, 255, 10, PD),
% image( 2, 2, [PA, PB, PC, PD], I), imageFlipH(I, I2).

% pixhex( 0, 0, "#FFFFFF", 20, PA), pixhex( 0, 1, "#FFFFFF", 20, PB), pixhex(1, 0, "#FFFFFF", 20, PC),
% pixhex( 1, 1, "#FFFFFF", 20, PD), pixhex( 2, 0, "#FFFFFF", 20, PE), pixhex(2, 1, "#FFFFFF", 20, PF),
% image( 3, 2, [PA, PB, PC, PD, PE, PF], I), imageFlipH(I, I2).

% pixbit( 0, 0, 1, 10, PA), pixbit( 0, 1, 1, 10, PB), 
% pixbit( 1, 0, 1, 10, PC), pixbit( 1, 1, 1, 10, PD), 
% image( 2, 2, [PA, PB, PC, PD], I), imageFlipH(I, I2).
%----////----%

% -> Se crea una imagen de 3x3 pixeles y se corta en una de 2x2 con solo la esquina inferior izquierda:
% 
% pixhex( 0, 0, "#FF0000", 20, PA), pixhex( 0, 1, "#FF0000", 20, PB), 
% pixhex( 0, 2, "#FF0000", 20, PC), pixhex( 1, 0, "#0000FF", 30, PD), 
% pixhex( 1, 1, "#0000FF", 4, PE), pixhex( 1, 2, "#0000FF", 4, PF), 
% pixhex( 2, 0, "#0000FF", 4, PG), pixhex( 2, 1, "#0000FF", 4, PH), 
% pixhex( 2, 2, "#0000FF", 4, PI), image( 3, 3, [PA, PB, PC, PD, PE, PF, PG, PH, PI], I), 
% imageCrop( I, 1, 1, 2, 2, I2), pixhex( 0, 0, "#0000FF", 4, PE2), 
% pixhex( 0, 1, "#0000FF", 4, PF2), pixhex( 1, 0, "#0000FF", 4, PH2), 
% pixhex( 1, 1, "#0000FF", 4, PI2), image( 2, 2, [PE2, PF2, PH2, PI2], I3).
%
% En el ejemplo anterior, “I2” debería ser una imagen con los mismos pixeles y dimensiones que “I3”

%----////----%
% pixrgb( 0, 0, 255, 255, 255, 10, PA), pixrgb( 0, 1, 255, 255, 255, 10, PB), pixrgb( 0, 2, 255, 255, 255, 10, PC),
% pixrgb( 1, 0, 255, 255, 255, 10, PD), pixrgb( 1, 1, 255, 255, 255, 10, PE), pixrgb( 1, 2, 255, 255, 255, 10, PF),
% pixrgb( 2, 0, 255, 255, 255, 10, PG), pixrgb( 2, 1, 255, 255, 255, 10, PH), pixrgb( 2, 2, 255, 255, 255, 10, PI),
% image( 2, 2, [PA, PB, PC, PD, PE, PF, PG, PH, PI], I), imageCrop( I, 1, 1, 2, 2, I2),
% pixrgb( 1, 1, 255, 255, 255, 10, PE2), pixrgb( 1, 2, 255, 255, 255, 10, PF),
% pixrgb( 2, 1, 255, 255, 255, 10, PH2), pixrgb( 2, 2, 255, 255, 255, 10, PI2), image( 2, 2, [PE2, PF2, PH2, PI2], I3).

% pixhex( 0, 0, "#FFFFFF", 20, PA), pixhex( 0, 1, "#FFFFFF", 20, PB), pixhex(0, 2, "#FFFFFF", 20, PC),
% pixhex( 1, 0, "#FFFFFF", 20, PD), pixhex( 1, 1, "#FFFFFF", 20, PE), pixhex(1, 2, "#FFFFFF", 20, PF),
% pixhex( 2, 0, "#FFFFFF", 20, PG), pixhex( 2, 1, "#FFFFFF", 20, PH), pixhex(2, 2, "#FFFFFF", 20, PI),
% image( 3, 3, [PA, PB, PC, PD, PE, PF], I), imageCrop( I, 1, 1, 2, 2, I2),
% pixhex( 1, 1, "#FFFFFF", 20, PE), pixhex(1, 2, "#FFFFFF", 20, PF),
% pixhex( 3, 1, "#FFFFFF", 20, PH), pixhex(2, 2, "#FFFFFF", 20, PI), image( 2, 2, [PE2, PF2, PH2, PI2], I3).

% pixbit( 0, 0, 1, 10, PA), pixbit( 0, 1, 1, 10, PB), pixbit( 0, 2, 1, 10, PC), 
% pixbit( 1, 0, 1, 10, PD), pixbit( 1, 1, 1, 10, PE), pixbit( 1, 2, 1, 10, PF),  
% pixbit( 2, 0, 1, 10, PG), pixbit( 2, 1, 1, 10, PH), pixbit( 2, 2, 1, 10, PI), 
% image( 2, 2, [PA, PB, PC, PD], I), imageCrop( I, 1, 1, 2, 2, I2), 
% pixbit( 1, 1, 1, 10, PE), pixbit( 1, 2, 1, 10, PF), 
% pixbit( 2, 1, 1, 10, PH), pixbit( 2, 2, 1, 10, PI), image( 2, 2, [PE2, PF2, PH2, PI2], I3). 
%----////----%

% Toma el píxel de la posición (0,1) que en la imagen original tiene 
% 
% los valores RGB (20, 20, 20) y lo reemplaza por otro píxel con valor RGB (54, 54, 54).
% pixrgb( 0, 0, 10, 10, 10, 10, P1), pixrgb( 0, 1, 20, 20, 20, 20, P2), 
% pixrgb( 1, 0, 30, 30, 30, 30, P3), pixrgb( 1, 1, 40, 40, 40, 40, P4), 
% image( 2, 2, [P1, P2, P3, P4], I1), pixrgb( 0, 1, 54, 54, 54, 20, P2_modificado), 
% imageChangePixel(I1, P2_modificado, I2).

%----////----%
% pixrgb( 0, 0, 255, 255, 255, 10, PA), pixrgb( 0, 1, 255, 255, 255, 10, PB),
% pixrgb( 1, 0, 255, 255, 255, 10, PC), pixrgb( 1, 1, 255, 255, 255, 10, PD),
% image( 2, 2, [PA, PB, PC, PD], I1), pixrgb( 0, 1, 25, 55, 5, P2_modificado),
% imageChangePixel(I1, P2_modificado, I2).

% pixhex( 0, 0, "#FFFFFF", 20, PA), pixhex( 0, 1, "#FFFFFF", 20, PB), 
% pixhex(0, 2, "#FFFFFF", 20, PC), pixhex( 1, 1, "#FFFFFF", 20, PD), 
% image( 2, 2, [PA, PB, PC, PD], I1), pixhex( 0, 1, "#000000", 200, P2_modificado),
% imageChangePixel(I1, P2_modificado, I2).

% pixbit( 0, 0, 1, 10, PA), pixbit( 0, 1, 1, 10, PB), 
% pixbit( 1, 0, 1, 10, PC), pixbit( 1, 1, 1, 10, PD), 
% image( 2, 2, [PA, PB, PC, PD], I1), pixbit( 0, 1, 0, 55, P2_modificado),
% imageChangePixel(I1, P2_modificado, I2).

%----////----%

% Se construye imagen de 2x2 con los primeros 2 pixeles con profundidad 10 y 
% los otros 2 con profundidad de 30, entonces al consultar “imageDepthLayers” se debería 
% obtener una lista con dos imágenes.
% 
% pixrgb( 0, 0, 33, 33, 33, 10, PA), pixrgb( 0, 1, 44, 44, 44, 10, PB), 
% pixrgb( 1, 0, 55, 55, 55, 30, PC), pixrgb( 1, 1, 66, 66, 66, 30, PD), 
% image( 2, 2, [PA, PB, PC, PD], I), imageDepthLayers (I, [PRIMERA, SEGUNDA]),
% pixrgb( 0, 0, 33, 33, 33, 10, PA2), pixrgb( 0, 1, 44, 44, 44, 10, PB2), 
% pixrgb( 1, 0, 255, 255, 255, 10, PC2), pixrgb( 1, 1, 255, 255, 255, 10, PD2), 
% image( 2, 2, [PA2, PB2, PC2, PD2], I2), pixrgb( 0, 0, 255, 255, 255, 30, PA3), 
% pixrgb( 0, 1, 255, 255, 255, 30, PB3), pixrgb( 1, 0, 55, 55, 55, 30, PC3), 
% pixrgb( 1, 1, 66, 66, 66, 30, PD3), image( 2, 2, [PA3, PB3, PC3, PD3], I3).
% En el ejemplo anterior, “I2” debería ser una imagen con los mismos pixeles y 
% dimensiones que “PRIMERA”. “I3” debería ser una imagen con los mismos pixeles y dimensiones que “SEGUNDA”.