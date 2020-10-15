CLOSE DATABASES
SELECT 1
USE liceo
GOTO TOP
SCATTER MEMO MEMVAR
DEFINE WINDOW scl FROM 2, 0 TO 22,  ;
       79 SHADOW COLOR SCHEME 5
MOVE WINDOW scl CENTER
ACTIVATE WINDOW scl
xd = 'DATOS DE IDENTIFICACION DEL PLANTEL'
@ 00, 00 SAY SPACE(((WCOLS() -  ;
  LEN(xd) - 6) / 2) + 3) + xd
@ 01, 00 TO 1, WCOLS()
@ 02, 00 SAY  ;
  'Nombre del Plantel    :                                             '
@ 03, 00 SAY  ;
  'R.I.F. :                                                 Telf.:          '
@ 04, 00 SAY  ;
  'Nombre del Director:                                 C.I.:           '
@ 05, 00 SAY  ;
  '                          Direcci¢n del Plantel                        '
@ 06, 00 SAY  ;
  '                                                                          '
@ 07, 00 TO 07, WCOLS()
@ 08, 00 SAY  ;
  'Firma de Documentos     :                                C.I.:           '
@ 09, 00 SAY  ;
  'Cargo que Desempe¤a     :                                                '
@ 10, 00 SAY  ;
  'Localidad :                                                '
@ 11, 00 SAY  ;
  'Nro. del Recibo :                                          '
@ 12, 00 SAY  ;
  'Nro. Control Matutino   :                                  '
@ 13, 00 SAY  ;
  'Nro. Control Vespertino :                                  '
@ 14, 00 SAY  ;
  'Nro. Control Nocturno   :                                  '
@ 15, 00 SAY  ;
  'Nro. Control Sabatino   :                                  '
@ 16, 00 SAY  ;
  'Nro. Control Sem. Anter.:                                  '
@ 17, 00 TO 17, WCOLS()
DO WHILE .T.
     @ 02, 24 SAY l_nombre
     @ 03, 12 SAY l_rif
     @ 04, 21 GET m.director  ;
       PICTURE  ;
       'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
     @ 04, 59 GET m.cedula  ;
       PICTURE 'XXXXXXXXXXXXXXX'
     @ 06, 00 GET m.direccion  ;
       PICTURE '@S77'
     @ 08, 26 GET m.nom_func  ;
       PICTURE  ;
       'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
     @ 08, 63 GET m.ced_func  ;
       PICTURE '@!S13'
     @ 09, 26 GET m.car_func  ;
       PICTURE  ;
       'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
     @ 10, 16 GET m.localidad  ;
       PICTURE '@!S45'
     @ 11, 26 GET m.recibo  ;
       PICTURE '99999999'
     @ 12, 26 GET m.controlmat  ;
       PICTURE '99999999'
     @ 13, 26 GET m.controlves  ;
       PICTURE '99999999'
     @ 14, 26 GET m.controlnoc  ;
       PICTURE '99999999'
     @ 15, 26 GET m.controlsab  ;
       PICTURE '99999999'
     @ 16, 26 GET m.reciboant  ;
       PICTURE '99999999'
     @ 17, 24 GET m.choice  ;
       DEFAULT 1 SIZE 1, 10, 10  ;
       PICTURE  ;
       '@*HT \<Aceptar;\<Cancelar'  ;
       COLOR SCHEME 5
     READ CYCLE
     IF LASTKEY() = 27
          RELEASE WINDOW scl
          CLOSE DATABASES
          RETURN
     ENDIF
     IF m.choice = 1
          SELECT 1
          DO WHILE .T.
               IF RLOCK()
                    GATHER MEMVAR  ;
                           MEMO
                    UNLOCK
                    EXIT
               ENDIF
          ENDDO
          l_unico = m.unico
          l_zona = m.zona
          l_distrito = m.distrito
          l_entidad = m.entidad
          l_dependen = m.dependenci
          l_telefono = m.telefono
          l_direccio = m.direccion
          l_director = m.director
          l_ceddirec = m.cedula
          l_contestu = m.contestudi
          l_cedestud = m.ceduestudi
          l_funciedu = m.funceducac
          l_cedfunci = m.cedueducac
          l_localida = m.localidad
          l_nom_func = m.nom_func
          l_ced_func = m.ced_func
          l_car_func = m.car_func
     ENDIF
     EXIT
ENDDO
RELEASE WINDOW scl
CLOSE DATABASES
RETURN
*
