CLOSE DATABASES
SET TALK OFF
SET CENTURY ON
CLOSE DATABASES
unidad1 = 'UNO'
unidad2 = 'DOS'
unidad3 = 'TRES'
unidad4 = 'CUATRO'
unidad5 = 'CINCO'
unidad6 = 'SEIS'
unidad7 = 'SIETE'
unidad8 = 'OCHO'
unidad9 = 'NUEVE'
SELECT 2
use mp&lapmat
SET ORDER TO cedcur
SELECT 1
USE datos AGAIN ALIAS datos ORDER  ;
    cedula
DEFINE WINDOW a1 FROM 03, 00 TO  ;
       15, 79 FLOAT NOCLOSE  ;
       SHADOW NOMINIMIZE COLOR  ;
       SCHEME 5
ACTIVATE WINDOW a1
xd = 'Constancia de Trabajo'
@ 0, 0 SAY SPACE(((WCOLS() -  ;
  LEN(xd) - 6) / 2) + 3) + xd
ng = CHR(27) + CHR(69)
ngn = CHR(27) + CHR(70)
unidad1 = 'UNO'
unidad2 = 'DOS'
unidad3 = 'TRES'
unidad4 = 'CUATRO'
unidad5 = 'CINCO'
unidad6 = 'SEIS'
unidad7 = 'SIETE'
unidad8 = 'OCHO'
unidad9 = 'NUEVE'
@ 01, 00 SAY  ;
  'ÕÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¸'
@ 02, 00 SAY  ;
  '³ C‚dula:              Apellidos y Nombres:                                 ³'
@ 03, 00 SAY  ;
  '³ Prest   sus  Servicios  como                                              ³'
@ 04, 00 SAY  ;
  '³ En la(s) Asignatura(s):                                                   ³'
@ 05, 00 SAY  ;
  '³ En el (los) Turno(s):                                                     ³'
@ 06, 00 SAY  ;
  '³ Desde el dia:                      Hasta:                                 ³'
@ 07, 00 SAY  ;
  '³ Fecha de Solicitud:                                                       ³'
@ 08, 00 SAY  ;
  'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
SAVE SCREEN TO hola
kdirector = l_director
m.cargo = 'DIRECTOR    '
ON KEY LABEL f8 keybo chr(13)
ON KEY LABEL f9 keybo chr(13)
ON KEY LABEL tab do validar with varread()
m.nac = 'V'
m.ced = SPACE(10)
valida1 = 'ABCDEFGHIJKLMNOPQRSTUVXYZ'
m.nombres = SPACE(30)
m.prest = 'a'
m.profe = SPACE(40)
m.asign = SPACE(40)
m.turnito = SPACE(41)
m.fechaini = DATE()
m.fechafin = SPACE(30)
m.fecha = DATE()
m.dem = SPACE(13)
DO WHILE .T.
     RESTORE SCREEN FROM hola
     @ 02, 10 GET m.nac PICTURE  ;
       '@!M V,E,D,R' MESSAGE  ;
       'ingrese la Nacionalidad del Alumno'
     @ 02, 11 SAY '-'
     @ 02, 12 GET m.ced PICTURE  ;
       '999999999' VALID  ;
       VAL(m.ced) >= 0 MESSAGE  ;
       'Ingrese el N£mero del Cedula del Alumno'  ;
       ERROR  ;
       'Ingrese al N£mero de Cedula'
     READ
     IF LASTKEY() = 27
          ON KEY LABEL tab
          RELEASE WINDOW a1
          CLOSE DATABASES
          RETURN
     ENDIF
     IF VAL(m.ced) <= 0
          = msgerro( ;
            'Ingrese el Nro. de Cedula' ;
            )
          LOOP
     ENDIF
     xcedula = m.nac + '-' +  ;
               m.ced
     @ 02, 44 GET m.nombres  ;
       PICTURE '@!S30'
     @ 03, 07 GET m.prest PICTURE  ;
       '@!M a,¢'
     @ 03, 31 GET m.profe PICTURE  ;
       '@!S40'
     @ 04, 26 GET m.asign PICTURE  ;
       '@!S40'
     @ 05, 24 GET m.turnito  ;
       PICTURE '@!s41'
     @ 06, 16 GET m.fechaini  ;
       PICTURE '@!E'
     @ 06, 44 GET m.fechafin  ;
       PICTURE '@!S30'
     @ 07, 22 GET m.fecha PICTURE  ;
       '@!E'
     READ
     IF LASTKEY() = 27
          ON KEY LABEL tab
          RELEASE WINDOW a1
          CLOSE DATABASES
          RETURN
     ENDIF
     p = printer()
     IF p = 2
          ON KEY LABEL tab
          RELEASE WINDOW a1
          CLOSE DATABASES
          RETURN
     ENDIF
     x = VAL(SUBSTR(DTOC(m.fecha),  ;
         8, 1))
     x = LTRIM(STR(x, 1))
     m.year=LOWER(unidad&x)+'.'
     xd = 'const1'
     SELECT 1
     SET DEVICE TO PRINTER
     y1 = ' '
     y2 = ALLTRIM(l_nombre)
     y3 = ALLTRIM(l_localida)
     y4 = 'INSCRITO EN EL M.E.' +  ;
          LTRIM(l_omad)
     @ 00, 00 SAY CHR(18) +  ;
       CHR(20) + ' ' + ng
     @ 01, 00 SAY SPACE(((80 -  ;
       LEN(y1) - 6) / 2) + 3) +  ;
       y1
     @ 02, 00 SAY SPACE(((80 -  ;
       LEN(y2) - 6) / 2) + 3) +  ;
       y2
     @ 03, 00 SAY SPACE(((80 -  ;
       LEN(y3) - 6) / 2) + 3) +  ;
       y3
     @ 04, 00 SAY SPACE(((80 -  ;
       LEN(y4) - 6) / 2) + 3) +  ;
       y4
     centro1 = 'C O N S T A N C I A    D E    T R A B A J O'
     @ 11, 00 SAY SPACE(((80 -  ;
       LEN(centro1) - 6) / 2) +  ;
       3) + centro1
     @ 18, 05 SAY  ;
       '  Qui‚n suscribe, Directora de la UNIDAD EDUCATIVA COLEGIO "SIGLO XXI",'
     @ 20, 05 SAY  ;
       'por  medio  de  la  presente  hace  constar  que  el(la)  ciudadano(a) '
     @ 22, 05 SAY  ;
       '                                               , titular de  la  C‚dula'
     @ 22, 05 SAY m.nombres
     @ 24, 05 SAY  ;
       'de Identidad Nro.            , prest  sus servicios en esta instituci¢n'
     @ 24, 23 SAY xcedula PICTURE  ;
       '!-9999999999'
     @ 24, 41 SAY m.prest
     @ 26, 05 SAY  ;
       'como                                           , en la(s) Asignatura(s)'
     @ 26, 11 SAY m.profe
     @ 28, 05 SAY  ;
       '                                            ,   en   el(los)   turno(s)'
     @ 28, 05 SAY m.asign
     @ 30, 05 SAY  ;
       '                                         ,  desde  el  d¡a             '
     @ 30, 05 SAY m.turnito
     @ 30, 65 SAY m.fechaini
     @ 32, 05 SAY  ;
       'hasta                     y  durante el cumplimiento de  sus  funciones'
     @ 32, 11 SAY m.fechafin
     IF m.prest = 'a'
          m.dem = 'ha demostrado'
     ELSE
          m.dem = 'demostr¢'
     ENDIF
     @ 34, 05 SAY  ;
       '              responsabilidad  y  eficiencia.'
     @ 34, 05 SAY m.dem
     @ 38, 05 SAY  ;
       '   Constancia  que  se  expide  a  solicitud  de  parte  interesada en'
     x = VAL(SUBSTR(DTOC(m.fecha),  ;
         8, 1))
     x = LTRIM(STR(x, 1))
     m.rtu = ALLTRIM(l_localida) +  ;
             ' , a los ' +  ;
             LOWER(conv(DAY(m.fecha))) +  ;
             ' d¡as del mes de ' +  ;
             LOWER(meses(m.fecha))
     m.rtu=m.rtu+' de mil novecientos noventa y '+lower(unidad&x)+'.'
     IF LEN(m.rtu) >= 68
          i = 71
          c = 1
          DO WHILE .T.
               IF SUBSTR(m.rtu, i,  ;
                  1) = ' '
                    m.rtu1 = SUBSTR(m.rtu,  ;
                             1,  ;
                             i)
                    rt = LEN(m.rtu1)
                    yu = 1 + rt
                    yu = 72 - yu
                    IF yu > 0
                         m.rtu1 =  ;
                          m.rtu1 +  ;
                          SPACE(yu)
                    ENDIF
                    lou1 = LEN(m.rtu)
                    k = i + lou1
                    m.rtu2 = SUBSTR(m.rtu,  ;
                             i +  ;
                             1,  ;
                             k) +  ;
                             SPACE(2)
                    EXIT
               ENDIF
               i = i - 1
          ENDDO
          m.rtu1 = ALLTRIM(m.rtu1)
          de = AT(',', m.rtu1)
          k = de
          DO WHILE .T.
               IF LEN(m.rtu1) >=  ;
                  70
                    EXIT
               ENDIF
               IF SUBSTR(m.rtu1,  ;
                  de, 1) = ' '
                    m.rtu1 = STUFF(m.rtu1,  ;
                             de,  ;
                             0,  ;
                             ' ' ;
                             )
                    de = de + 2
               ENDIF
               IF LEN(m.rtu1) =  ;
                  de
                    de = AT(',',  ;
                         m.rtu1)
               ENDIF
               de = de + 1
          ENDDO
          @ 40, 05 SAY m.rtu1
          @ 42, 05 SAY m.rtu2
     ELSE
          @ 40, 05 SAY m.rtu
     ENDIF
     xd = '_____________________________'
     @ 47, 00 SAY SPACE(((80 -  ;
       LEN(xd) - 6) / 2) + 3) +  ;
       xd
     xd = l_nom_func
     @ 48, 00 SAY SPACE(((80 -  ;
       LEN(xd) - 6) / 2) + 3) +  ;
       xd
     xd = l_car_func
     @ 49, 00 SAY SPACE(((80 -  ;
       LEN(xd) - 6) / 2) + 3) +  ;
       xd
     @ 50, 01 SAY CHR(18) +  ;
       CHR(20) + ' ' + ngn
     SET DEVICE TO SCREEN
     m.nac = 'V'
     m.ced = SPACE(10)
     m.prest = 'a'
     m.profe = SPACE(40)
     m.asign = SPACE(40)
     m.turnito = SPACE(25)
     m.fechaini = DATE()
     m.fechafin = SPACE(30)
     m.fecha = DATE()
ENDDO
ON KEY LABEL tab
CLOSE DATABASES
RELEASE WINDOW a1
RETURN
*
FUNCTION conv
PARAMETER xmonto
PRIVATE ALL
xnumero = LTRIM(STR(xmonto, 2))
unidad0 = ' '
unidad1 = 'UN'
unidad2 = 'DOS'
unidad3 = 'TRES'
unidad4 = 'CUATRO'
unidad5 = 'CINCO'
unidad6 = 'SEIS'
unidad7 = 'SIETE'
unidad8 = 'OCHO'
unidad9 = 'NUEVE'
decena00 = ' '
decena10 = 'DIEZ'
decena11 = 'ONCE'
decena12 = 'DOCE'
decena13 = 'TRECE'
decena14 = 'CATORCE'
decena15 = 'QUINCE'
decena16 = 'DIECISEIS'
decena17 = 'DIECISIETE'
decena18 = 'DIECIOCHO'
decena19 = 'DIECINUEVE'
decena20 = 'VEINTE'
decena30 = 'TREINTA'
decena40 = 'CUARENTA'
decena50 = 'CINCUENTA'
decena60 = 'SESENTA'
decena70 = 'SETENTA'
decena80 = 'OCHENTA'
decena90 = 'NOVENTA'
b2 = SUBSTR(xnumero, 1, 2)
IF VAL(b2) <= 10
     b2 = SUBSTR(xnumero, 1, 1)
     let1=unidad&b2
ELSE
     b2 = SUBSTR(xnumero, 1, 1)
     b3 = SUBSTR(xnumero, 2, 1)
     IF VAL(b2 + b3) >= 20
          t = b2 + '0'
          let1=trim(decena&t)
          let1=let1+iif(b3<>'0',' Y '+unidad&b3,unidad&b3)
     ELSE
          IF VAL(b2 + b3) < 10
               let1=unidad&b3
          ELSE
               t = b2 + b3
               let1=trim(decena&t)
          ENDIF
     ENDIF
ENDIF
RETURN let1
*
PROCEDURE validar
PARAMETER xvar
DO CASE
     CASE xvar = 'NAC' .OR. xvar =  ;
          'CED'
          ON KEY LABEL tab
          xcedula = m.nac + '-' +  ;
                    m.ced
          SELECT 1
          xcedula = busqueda()
          m.nac = SUBSTR(xcedula,  ;
                  1, 1)
          m.ced = SUBSTR(xcedula,  ;
                  3, 10)
          SHOW GET m.nac
          SHOW GET m.ced
ENDCASE
ON KEY LABEL tab do validar with varread()
RETURN
*
