      *   Area de datos de Entrada
          02 LK-STRING-I.
             05 LK-FUNCION-I                           PIC X(01).
             05 LK-FILLER-I                            PIC X(6000).

      *      Funcion 1: LENGTH
             05 LK-FUNCION-1-I REDEFINES LK-FILLER-I.
                15 LK-TEXTO-1-I                        PIC X(3000).
                15 FILLER                              PIC X(3000).
      *      Funcion 2: Mayuscula
             05 LK-FUNCION-2-I REDEFINES LK-FILLER-I.
                15 LK-TEXTO-2-I                        PIC X(3000).
               15 FILLER                              PIC X(3000).
      *      Funcion 3: Concatentar
             05 LK-FUNCION-3-I REDEFINES LK-FILLER-I.
                15 LK-TEXTO1-3-I                      PIC X(3000).
                15 LK-TEXTO2-3-I                      PIC X(3000).

      *   Area de datos de Salida
          02 LK-STRING-O.
             05 LK-FILLER-O                            PIC X(6000).
      *      Funcion 1: LENGTH
             05 LK-FUNCION-1-O REDEFINES LK-FILLER-O.
                15 LK-LEN-O                            PIC 9(04).
                15 FILLER                              PIC X(5996).
      *      Funcion 2: Mayuscula
             05 LK-FUNCION-2-O REDEFINES LK-FILLER-O.
                15 LK-TEXTO-2-O                        PIC X(3000).
                15 FILLER                              PIC X(3000).
      *      Funcion 3: Concatentar
             05 LK-FUNCION-3-O REDEFINES LK-FILLER-O.
                15 LK-TEXTO-3-O                        PIC X(6000).
      *      Error
             05 LK-ERROR-O REDEFINES LK-FILLER-O.
                15 LK-CODIGO-ERROR-O                   PIC X(08).
                15 LK-DESCRIPCION-ERROR-O              PIC X(100).
                15 FILLER                              PIC X(5892).
