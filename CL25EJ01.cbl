      ******************************************************************
      * Author: Gabriela C Rodriguez
      * Date:
      * Purpose: Funciones de string
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CL25EJ01.
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *----------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01 WS-VARIABLES.
           02 WS-FUNCION                    PIC X(01) VALUE SPACE.
           02 WS-LEGAJO-AUX                 PIC 9(08) VALUE ZEROS.
           02 WS-ESTADO-AUX                 PIC X(02) VALUE SPACES.

      * ESTRUCTURA DE DATOS PARA COMUNICARSE CON LA RUTINA CLSTRING
       01 LK-STRING.
          COPY CLSTRING.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

       MAIN-PROCEDURE.

           PERFORM 1000-INICIAR-PROGRAMA
              THRU 1000-INICIAR-PROGRAMA-EXIT.

           PERFORM 2000-PROCESAR-FUNCION
              THRU 2000-PROCESAR-FUNCION-EXIT.

           STOP RUN.
      *----------------------------------------------------------------*
      * PROCESO DE INICIALIZACION DEL PROGRAMA                         *
      *----------------------------------------------------------------*
       1000-INICIAR-PROGRAMA.

           DISPLAY '------------------------------------------------'.
           DISPLAY '  *   Funcion 1: LENGTH                         '.
           DISPLAY '  *   Funcion 2: Mayuscula                      '.
           DISPLAY '  *   Funcion 3: Concatentar                    '.
           DISPLAY '------------------------------------------------'.

           DISPLAY 'Ingresar funcion:'.
           ACCEPT  WS-FUNCION.

           DISPLAY '------------------------------------------------'.

       1000-INICIAR-PROGRAMA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * PROCESAR FUNCION                                               *
      *----------------------------------------------------------------*
       2000-PROCESAR-FUNCION.

           EVALUATE WS-FUNCION
               WHEN '1'
                    PERFORM 2100-FUNCION-LENGHT
                       THRU 2100-FUNCION-LENGHT-EXIT
               WHEN '2'
                    PERFORM 2200-FUNCION-MAYUSCULA
                       THRU 2200-FUNCION-MAYUSCULA-EXIT
               WHEN '3'
                    PERFORM 2300-FUNCION-CONCATENAR
                       THRU 2300-FUNCION-CONCATENAR-EXIT
               WHEN OTHER
                    DISPLAY 'La funcion ingresada es invalida'
           END-EVALUATE.

       2000-PROCESAR-FUNCION-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * FUNCION PARA CONTAR LA LONGITUD DE UNA CADENA DE CARACTERES    *
      *----------------------------------------------------------------*
       2100-FUNCION-LENGHT.

           INITIALIZE LK-STRING.
           MOVE WS-FUNCION                   TO LK-FUNCION-I.

           DISPLAY 'Ingresar cadena de caracter:'
           ACCEPT  LK-TEXTO-1-I.
           DISPLAY '------------------------------------------------'.

           CALL 'CLSTRING' USING LK-STRING.

      *    Evaluar codigo de retorno RETURN-CODE
           IF RETURN-CODE EQUAL ZEROES
              DISPLAY 'Resultado: ' LK-LEN-O
           ELSE
      *       Mostrar codigo y descripcion de error
              DISPLAY "ERROR"
              DISPLAY "RETURN-CODE           : " RETURN-CODE
              DISPLAY "LK-CODIGO-ERROR-O     : " LK-CODIGO-ERROR-O
              DISPLAY "LK-DESCRIPCION-ERROR-O: " LK-DESCRIPCION-ERROR-O
           END-IF.

       2100-FUNCION-LENGHT-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * FUNCION PARA CONVERTIR EN MAYUSCULA UNA CADENA DE CARACTERES   *
      *----------------------------------------------------------------*
       2200-FUNCION-MAYUSCULA.

           INITIALIZE LK-STRING.
           MOVE WS-FUNCION                   TO LK-FUNCION-I.

           DISPLAY 'Ingresar cadena de caracter:'
           ACCEPT  LK-TEXTO-2-I.
           DISPLAY '------------------------------------------------'.

           CALL 'CLSTRING' USING LK-STRING.

      *    Evaluar codigo de retorno RETURN-CODE
           IF RETURN-CODE EQUAL ZEROES
              DISPLAY 'Resultado: ' LK-TEXTO-2-O
           ELSE
      *       Mostrar codigo y descripcion de error
              DISPLAY "ERROR"
              DISPLAY "RETURN-CODE           : " RETURN-CODE
              DISPLAY "LK-CODIGO-ERROR-O     : " LK-CODIGO-ERROR-O
              DISPLAY "LK-DESCRIPCION-ERROR-O: " LK-DESCRIPCION-ERROR-O
           END-IF.

       2200-FUNCION-MAYUSCULA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * FUNCION PARA CONCATENAR DOS CADENAS DE CARACTERES              *
      *----------------------------------------------------------------*
       2300-FUNCION-CONCATENAR.

           INITIALIZE LK-STRING.
           MOVE WS-FUNCION                   TO LK-FUNCION-I.

           DISPLAY 'Ingresar cadena 1:'
           ACCEPT  LK-TEXTO1-3-I.
           DISPLAY 'Ingresar cadena 2:'
           ACCEPT  LK-TEXTO2-3-I.
           DISPLAY '------------------------------------------------'.

           CALL 'CLSTRING' USING LK-STRING.

      *    Evaluar codigo de retorno RETURN-CODE
           IF RETURN-CODE EQUAL ZEROES
              DISPLAY 'Resultado: ' LK-TEXTO-3-O
           ELSE
      *       Mostrar codigo y descripcion de error
              DISPLAY "ERROR"
              DISPLAY "RETURN-CODE           : " RETURN-CODE
              DISPLAY "LK-CODIGO-ERROR-O     : " LK-CODIGO-ERROR-O
              DISPLAY "LK-DESCRIPCION-ERROR-O: " LK-DESCRIPCION-ERROR-O
           END-IF.

       2300-FUNCION-CONCATENAR-EXIT.
      *----------------------------------------------------------------*

       END PROGRAM CL25EJ01.
