CLOSE DATABASES
SET TALK OFF
SET DEVICE TO SCREEN
SET CENTURY ON
SELECT 1
USE concepto AGAIN ALIAS concepto  ;
    ORDER codigo
SELECT 2
USE detacobro AGAIN ALIAS  ;
    detcobro ORDER codigo
SELECT 3
USE plancobr AGAIN ALIAS plancobr  ;
    ORDER codigo
DEFINE WINDOW _r7a0vjj0c FROM 00,  ;
       00 TO 24, 79 GROW FLOAT  ;
       CLOSE ZOOM SHADOW TITLE  ;
       'Registro de Planes de Cobranza'  ;
       MINIMIZE COLOR SCHEME 5
ACTIVATE WINDOW _r7a0vjj0c
SET CLOCK OFF
@ 00, 00 SAY  ;
  'ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»'
@ 01, 00 SAY  ;
  'º Codigo de Plan:       Descripci¢n:                                         º'
@ 02, 00 SAY  ;
  'º Nro. Cuotas:     Fecha Inicio :                                            º'
@ 03, 00 SAY  ;
  'º                                                                            º'
@ 04, 00 SAY  ;
  'ÌÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹'
@ 05, 00 SAY  ;
  'º Cuotas ³ Conceptos de Cobro                   ³  Vencimto  ³   M o n t o   º'
@ 06, 00 SAY  ;
  'ÇÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶'
@ 07, 00 SAY  ;
  'º        ³                                      ³            ³               º'
@ 08, 00 SAY  ;
  'º        ³                                      ³            ³               º'
@ 09, 00 SAY  ;
  'º        ³                                      ³            ³               º'
@ 10, 00 SAY  ;
  'º        ³                                      ³            ³               º'
@ 11, 00 SAY  ;
  'º        ³                                      ³            ³               º'
@ 12, 00 SAY  ;
  'º        ³                                      ³            ³               º'
@ 13, 00 SAY  ;
  'º        ³                                      ³            ³               º'
@ 14, 00 SAY  ;
  'º        ³                                      ³            ³               º'
@ 15, 00 SAY  ;
  'º        ³                                      ³            ³               º'
@ 16, 00 SAY  ;
  'º        ³                                      ³            ³               º'
@ 17, 00 SAY  ;
  'º        ³                                      ³            ³               º'
@ 18, 00 SAY  ;
  'º        ³                                      ³            ³               º'
@ 19, 00 SAY  ;
  'ÇÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶'
@ 20, 00 SAY  ;
  'º                                                            ³               º'
@ 21, 00 SAY  ;
  'º Ins=Insertar/Cuota Del=Suprimir Enter=Modificar  Totales ->³<             >º'
@ 22, 00 SAY  ;
  'ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼'
SAVE SCREEN TO polo
mcodigo = SPACE(5)
mdescrip = SPACE(45)
ON KEY LABEL tab do validar with varread()
DO WHILE .T.
     RESTORE SCREEN FROM polo
     @ 01, 18 GET mcodigo PICTURE  ;
       '@!'
     READ
     IF LASTKEY() = 27
          ON KEY LABEL tab
          ON KEY LABEL del
          RELEASE WINDOW  ;
                  _r7a0vjj0c
          CLOSE DATABASES
          RETURN
     ENDIF
     IF mcodigo = SPACE(5)
          = msgerro( ;
            'El Codigo del Plan de Cobro esta en blanco, verifique' ;
            )
          LOOP
     ENDIF
     SELECT 3
     SET ORDER TO lapcod
     IF  .NOT. SEEK(lapmat +  ;
         mcodigo)
          = msgerro( ;
            'Este Plan de Cobro no esta Registado' ;
            )
          opc = acepta( ;
                'Desea Registrar esta Plan de Cobro' ;
                )
          IF opc = 2
               LOOP
          ENDIF
     ENDIF
     mdescrip = descripcio
     mnrocuotas = nrocuotas
     mfecha_in = fecha_in
     mfecha_fi = GOMONTH(mfecha_in,  ;
                 mnrocuotas)
     mprein = cod_preins
     montopre = pre_inscri
     @ 01, 38 GET mdescrip  ;
       PICTURE '@!S38'
     IF  .NOT. FOUND()
          @ 02, 16 GET mnrocuotas  ;
            DEFAULT 6 PICTURE  ;
            '99' VALID mnrocuotas >=  ;
            1 .AND. mnrocuotas <=  ;
            6 ERROR  ;
            'El N£mero del Cuotas Anuales es Invalido- Maximo de 6'
     ELSE
          @ 02, 16 SAY mnrocuotas  ;
            PICTURE '99' COLOR W+/ ;
            W 
     ENDIF
     @ 02, 35 GET mfecha_in  ;
       PICTURE '@E'
     READ
     IF LASTKEY() = 27
          p = salida()
          IF p = 2
               ON KEY LABEL tab
               ON KEY LABEL del
               CLOSE DATABASES
               RELEASE WINDOW
               RETURN
          ENDIF
          LOOP
     ENDIF
     IF mfecha_in = CTOD( ;
        '  /  /  ')
          = msgerro( ;
            'La Fecha de Inicio de Plan de Cobro esta en blanco' ;
            )
          LOOP
     ENDIF
     FOR mm = 1 TO 12
          lm = LTRIM(STR(mm))
          mfechv&lm=ctod("  /  /  ")
          mcuota&lm=0
          FOR kk = 1 TO 10
               lk = LTRIM(STR(kk))
               lb = lm + lk
               cod&lb=space(2)
               mon&lb=0.00
               mdes&lb=space(40)
          ENDFOR
     ENDFOR
     SAVE SCREEN TO mcuot
     swp = 0
     cuantos = 0
     SELECT 2
     SET ORDER TO lapcod
     IF SEEK(lapmat + mcodigo)
          swp = 1
          RELEASE datos
          DIMENSION datos( 50)
          EXTERNAL ARRAY datos
          STORE ' ' TO datos
          i = 1
          suma = 0
          DO WHILE  .NOT. EOF()
               IF lapso <> lapmat
                    SELECT 2
                    SKIP
                    LOOP
               ENDIF
               IF codigo <>  ;
                  mcodigo
                    EXIT
               ENDIF
               SELECT 2
               mcuota = STR(cuota,  ;
                        7)
               u = 1
               FOR m = 1 TO 10
                    lk = LTRIM(STR(m))
                    if concepto&lk=space(2)
                         LOOP
                    ENDIF
                    mcon=concepto&lk
                    mfec = DTOC(fecha_ven)
                    mfec = ' ' +  ;
                           mfec +  ;
                           ' '
                    mmon=str(monto&lk,15,2)
                    suma=suma+monto&lk
                    SELECT 1
                    IF  .NOT.  ;
                        SEEK(mcon)
                         mdesc = SPACE(35)
                    ELSE
                         mdesc = SUBSTR(descripcio,  ;
                                 1,  ;
                                 35)
                    ENDIF
                    SELECT 2
                    IF u = 1
                         datos(  ;
                              i) =  ;
                              mcuota +  ;
                              '³' +  ;
                              mcon +  ;
                              ' ' +  ;
                              mdesc +  ;
                              '³' +  ;
                              mfec +  ;
                              '³' +  ;
                              mmon +  ;
                              '³'
                    ELSE
                         mcuota =  ;
                          SPACE(6)
                         mfec = SPACE(12)
                         mdatos =  ;
                          mcuota +  ;
                          '³' +  ;
                          mcon +  ;
                          ' ' +  ;
                          mdesc +  ;
                          '³' +  ;
                          mfec +  ;
                          '³' +  ;
                          mmon +  ;
                          '³'
                         datos(  ;
                              i) =  ;
                              datos(i) +  ;
                              mdatos
                    ENDIF
                    u = u + 1
                    i = i + 1
               ENDFOR
               SELECT 2
               SKIP
          ENDDO
          @ 21, 63 GET suma  ;
            PICTURE  ;
            '99,999,999.99' COLOR , ;
            W+/W 
          CLEAR GETS
     ELSE
          RELEASE datos
          DIMENSION datos( 50)
          EXTERNAL ARRAY datos
          STORE ' ' TO datos
          FOR mm = 1 TO  ;
              mnrocuotas
               lm = LTRIM(STR(mm))
               mfechav = GOMONTH(mfecha_in,  ;
                         mm)
               mcuota = STR(mm,  ;
                        7)
               mfec = DTOC(mfechav)
               mfec = ' ' + mfec +  ;
                      ' '
               mcon = '99'
               SELECT 1
               IF  .NOT.  ;
                   SEEK(mcon)
                    mdesc = SPACE(35)
               ELSE
                    mdesc = SUBSTR(descripcio,  ;
                            1,  ;
                            35)
               ENDIF
               mmon = STR(0, 15)
               mdatos = mcuota +  ;
                        '³' +  ;
                        mcon +  ;
                        ' ' +  ;
                        mdesc +  ;
                        '³' +  ;
                        mfec +  ;
                        '³' +  ;
                        mmon +  ;
                        '³'
               datos( mm) =  ;
                    mdatos
          ENDFOR
          i = mnrocuotas + 1
     ENDIF
     SELECT 2
     DO modifica
     opc = acepta( ;
           'Desea Guardar este Plan de Cobro' ;
           )
     IF opc = 1
          SELECT 3
          SEEK lapmat + mcodigo
          mfecha_fi = GOMONTH(mfecha_in,  ;
                      mnrocuotas)
          DO WHILE .T.
               IF FLOCK()
                    IF  .NOT.  ;
                        FOUND()
                         APPEND BLANK
                    ENDIF
                    REPLACE lapso  ;
                            WITH  ;
                            lapmat,  ;
                            codigo  ;
                            WITH  ;
                            mcodigo,  ;
                            descripcio  ;
                            WITH  ;
                            mdescrip
                    REPLACE nrocuotas  ;
                            WITH  ;
                            mnrocuotas,  ;
                            fecha_in  ;
                            WITH  ;
                            mfecha_in,  ;
                            fecha_fi  ;
                            WITH  ;
                            mfecha_fi
                    REPLACE cod_preins  ;
                            WITH  ;
                            mprein,  ;
                            pre_inscri  ;
                            WITH  ;
                            montopre
                    UNLOCK
                    EXIT
               ENDIF
          ENDDO
          SELECT 2
          SET ORDER TO lapcod
          LOCATE FOR lapso =  ;
                 lapmat .AND.  ;
                 codigo =  ;
                 mcodigo
          DO WHILE  .NOT. EOF()
               IF lapmat <> lapso
                    SELECT 2
                    SKIP
                    LOOP
               ENDIF
               IF codigo <>  ;
                  mcodigo
                    SELECT 2
                    SKIP
                    LOOP
               ENDIF
               IF cuota >  ;
                  mnrocuotas
                    DELETE
               ENDIF
               SKIP
          ENDDO
          swm = 0
          FOR kk = 1 TO cuantos
               IF swm = 0
                    mm = VAL(LEFT(datos(kk),  ;
                         7))
                    mfechav = CTOD(SUBSTR(datos(kk),  ;
                              49,  ;
                              10))
                    ii = 1
                    LOCATE FOR  ;
                           lapso =  ;
                           lapmat  ;
                           .AND.  ;
                           codigo =  ;
                           mcodigo  ;
                           .AND.  ;
                           cuota =  ;
                           mm
                    DO WHILE .T.
                         IF FLOCK()
                              IF   ;
                               .NOT.  ;
                               FOUND()
                                   APPEND BLANK
                              ENDIF
                              REPLACE  ;
                               codigo  ;
                               WITH  ;
                               mcodigo,  ;
                               lapso  ;
                               WITH  ;
                               lapmat
                              REPLACE  ;
                               cuota  ;
                               WITH  ;
                               mm,  ;
                               fecha_ven  ;
                               WITH  ;
                               mfechav
                              FOR  ;
                               qq =  ;
                               1  ;
                               TO  ;
                               10
                                   lq = LTRIM(STR(qq))
                                   replace concepto&lq with space(2), monto&lq with 0.00
                              ENDFOR
                              UNLOCK
                              EXIT
                         ENDIF
                    ENDDO
                    swm = 1
               ENDIF
               IF VAL(LEFT(datos(kk),  ;
                  7)) > mm
                    ii = 1
                    mm = VAL(LEFT(datos(kk),  ;
                         7))
                    mfechav = CTOD(SUBSTR(datos(kk),  ;
                              49,  ;
                              10))
                    LOCATE FOR  ;
                           lapso =  ;
                           lapmat  ;
                           .AND.  ;
                           codigo =  ;
                           mcodigo  ;
                           .AND.  ;
                           cuota =  ;
                           mm
                    DO WHILE .T.
                         IF FLOCK()
                              IF   ;
                               .NOT.  ;
                               FOUND()
                                   APPEND BLANK
                              ENDIF
                              REPLACE  ;
                               codigo  ;
                               WITH  ;
                               mcodigo,  ;
                               lapso  ;
                               WITH  ;
                               lapmat
                              REPLACE  ;
                               cuota  ;
                               WITH  ;
                               mm,  ;
                               fecha_ven  ;
                               WITH  ;
                               mfechav
                              FOR  ;
                               qq =  ;
                               1  ;
                               TO  ;
                               10
                                   lq = LTRIM(STR(qq))
                                   replace concepto&lq with space(2), monto&lq with 0.00
                              ENDFOR
                              UNLOCK
                              EXIT
                         ENDIF
                    ENDDO
               ENDIF
               lm = LTRIM(STR(ii))
               mcon = SUBSTR(datos(kk),  ;
                      9, 2)
               mmonto = VAL(SUBSTR(datos(kk),  ;
                        63, 13))
               DO WHILE .T.
                    IF RLOCK()
                         replace concepto&lm;
with mcon, monto&lm with mmonto
                         UNLOCK
                         EXIT
                    ENDIF
               ENDDO
               ii = ii + 1
          ENDFOR
     ENDIF
     SELECT 3
     SCATTER BLANK MEMO MEMVAR
     mcodigo = SPACE(5)
     mdescrip = SPACE(45)
ENDDO
*
FUNCTION mconcepto
PARAMETER mcod, mk
IF mk >= 2
     IF mcod = SPACE(2)
          RETURN .T.
     ENDIF
ENDIF
SELECT 1
IF  .NOT. SEEK(mcod)
     mcod = boxfield('CONCEPTOS', ;
            'CODIGO')
ENDIF
mdes = ALLTRIM(descripcio)
@ lin, 12 SAY mcod PICTURE '@!'  ;
  COLOR W+/W 
@ lin, 15 SAY mdes PICTURE  ;
  '@!S31'
cod&lb=mcod
show get cod&lb
RETURN .T.
*
PROCEDURE modifica
m.da = 1
x = i - 1
ON KEY LABEL del do borra with varread()
ON KEY LABEL ins do anexa with varread()
DO WHILE .T.
     @06,1 get m.da  picture "@&N";
 valid fnadd(m.da) from datos range 1,x;
 size 13,79 color w+/bg,w+/w+,w/bg,,,,
     READ CYCLE SHOW slist_show()
     EXIT
ENDDO
ON KEY LABEL del
ON KEY LABEL ins
cuantos = x
RETURN
*
FUNCTION fnadd
PARAMETER npicked
ON KEY LABEL ins
ON KEY LABEL del
mdatos = datos(npicked)
SAVE SCREEN TO salva
mcon = SUBSTR(mdatos, 9, 2)
mdes = SUBSTR(mdatos, 12, 35)
mfechav = SUBSTR(mdatos, 49, 10)
mmonto = SUBSTR(mdatos, 63, 13)
mmonto = VAL(mmonto)
@ 20, 11 SAY mdes PICTURE '@!'  ;
  COLOR W+/W 
@ 20, 08 GET mcon PICTURE '@!'  ;
  VALID detalle(mcon)
IF mfechav <> SPACE(10)
     @ 20, 49 GET mfechav PICTURE  ;
       '@E'
ENDIF
@ 20, 63 GET mmonto PICTURE  ;
  '99,999,999.99' VALID mmonto >  ;
  100 ERROR  ;
  'El Monto del Concepto es invalido'
READ
SELECT 1
IF  .NOT. SEEK(mcon)
     mcon = boxfield('CONCEPTOS', ;
            'CODIGO')
ENDIF
mdes = SUBSTR(descripcio, 1, 35)
mcon = codigo
@ 20, 08 GET mcon PICTURE '@!'  ;
  VALID mconcepto(mcon)
@ 20, 11 SAY mdes PICTURE '@!'  ;
  COLOR W+/W 
CLEAR GETS
mmonto = STR(mmonto, 13, 2)
mdatos = STUFF(mdatos, 9, 3, mcon +  ;
         ' ')
mdatos = STUFF(mdatos, 12, 35,  ;
         mdes)
mdatos = STUFF(mdatos, 49, 10,  ;
         mfechav)
mdatos = STUFF(mdatos, 63, 13,  ;
         mmonto)
datos( npicked) = mdatos
RESTORE SCREEN FROM salva
suma = 0.00 
FOR i = 1 TO x
     suma = suma +  ;
            VAL(SUBSTR(datos(i),  ;
            63, 13))
ENDFOR
@ 21, 63 GET suma PICTURE  ;
  '99,999,999.99' COLOR ,W+/W 
CLEAR GETS
SHOW GET m.da
ON KEY LABEL del do borra with varread()
ON KEY LABEL ins do anexa with varread()
RETURN .T.
*
FUNCTION borra
PARAMETER npicked
ON KEY LABEL ins
ON KEY LABEL del
pp=&npicked
mdatos = datos(pp)
IF VAL(LEFT(mdatos, 7)) <> 0
     IF mnrocuotas <= 1
          = msgerro( ;
            'Este Plan debe contener al menos una cuota mensual' ;
            )
          m.da = m.da + 1
          IF m.da > x
               m.da = x
          ENDIF
          SHOW GET m.da
          ON KEY LABEL del do borra with;
varread()
          ON KEY LABEL ins do anexa with;
varread()
          RETURN .T.
     ENDIF
     opc = acepta( ;
           'Desea Eliminar esta Cuota' ;
           )
     IF opc = 1
          = msgerro( ;
            'Este Plan de Cobro es de ' +  ;
            LTRIM(STR(mnrocuotas)) +  ;
            ' Cuotas')
          opc = acepta( ;
                'Desea actualizar este plan' ;
                )
          IF opc = 1
               = ADEL(datos, pp)
               x = x - 1
               mnrocuotas = mnrocuotas -  ;
                            1
               @ 02, 16 SAY  ;
                 mnrocuotas  ;
                 PICTURE '99'  ;
                 COLOR W+/W 
               xx = 1
               FOR ii = 1 TO x
                    mdatos = datos(ii)
                    IF VAL(LEFT(mdatos,  ;
                       7)) <> 0
                         mdatos =  ;
                          STUFF(mdatos,  ;
                          1, 7,  ;
                          STR(xx,  ;
                          7))
                         datos(  ;
                              ii) =  ;
                              mdatos
                         xx = xx +  ;
                              1
                    ENDIF
               ENDFOR
          ENDIF
     ENDIF
     m.da = m.da + 1
     IF m.da > x
          m.da = x
     ENDIF
     SHOW GET m.da
     SHOW GETS
     suma = 0.00 
     FOR i = 1 TO x
          suma = suma +  ;
                 VAL(SUBSTR(datos(i),  ;
                 63, 13))
     ENDFOR
     @ 21, 63 GET suma PICTURE  ;
       '99,999,999.99' COLOR ,W+/ ;
       W 
     CLEAR GETS
     ON KEY LABEL del do borra with varread()
     ON KEY LABEL ins do anexa with varread()
     RETURN .T.
ELSE
     IF x <= 1
          = msgerro( ;
            'Este Plan debe contener al menos una cuota mensual' ;
            )
          m.da = m.da + 1
          IF m.da > x
               m.da = x
          ENDIF
          SHOW GET m.da
          ON KEY LABEL del do borra with;
varread()
          ON KEY LABEL ins do anexa with;
varread()
          RETURN .T.
     ENDIF
ENDIF
opc = acepta( ;
      'Desea Eliminar este concepto' ;
      )
IF opc = 1
     = ADEL(datos, pp)
     x = x - 1
ENDIF
suma = 0.00 
FOR i = 1 TO x
     suma = suma +  ;
            VAL(SUBSTR(datos(i),  ;
            63, 13))
ENDFOR
@ 21, 63 GET suma PICTURE  ;
  '99,999,999.99' COLOR ,W+/W 
CLEAR GETS
SHOW GETS
SHOW GET m.da
ON KEY LABEL del do borra with varread()
ON KEY LABEL ins do anexa with varread()
RETURN .T.
*
FUNCTION anexa
PARAMETER npicked
ON KEY LABEL ins
ON KEY LABEL del
pp=&npicked
mdatos = datos(pp)
IF VAL(LEFT(mdatos, 7)) = 0
     opc = acepta( ;
           'Desea Insertar un Nuevo detalle' ;
           )
     IF opc = 2
          ON KEY LABEL del do borra with;
varread()
          ON KEY LABEL ins do anexa with;
varread()
          RETURN .T.
     ENDIF
     pp=&npicked
     IF pp = 1
          = AINS(datos, pp + 1)
          pp = pp + 1
     ELSE
          = AINS(datos, pp + 1)
          pp = pp + 1
     ENDIF
     x = x + 1
     mdatos = datos(pp)
     mcuota = SPACE(7)
     mfec = SPACE(12)
     mdesc = SPACE(35)
     mmon = SPACE(15)
     mcon = SPACE(2)
     mdatos = mcuota + '³' + mcon +  ;
              ' ' + mdesc + '³' +  ;
              mfec + '³' + mmon +  ;
              '³'
     datos( pp) = mdatos
     SELECT 3
     xx = 1
     FOR ii = 1 TO x
          mdatos = datos(ii)
          IF VAL(LEFT(mdatos, 7)) <>  ;
             0
               mfechav = DTOC(GOMONTH(mfecha_in,  ;
                         xx))
               mdatos = STUFF(mdatos,  ;
                        49, 10,  ;
                        mfechav)
               mdatos = STUFF(mdatos,  ;
                        1, 7,  ;
                        STR(xx,  ;
                        7))
               datos( ii) =  ;
                    mdatos
               xx = xx + 1
          ENDIF
     ENDFOR
     SELECT 2
     SHOW GET m.da
     SHOW GETS
     suma = 0.00 
     FOR i = 1 TO x
          suma = suma +  ;
                 VAL(SUBSTR(datos(i),  ;
                 63, 13))
     ENDFOR
     SELECT 2
     @ 21, 63 GET suma PICTURE  ;
       '99,999,999.99' COLOR ,W+/ ;
       W 
     CLEAR GETS
     SHOW GET m.da
     DO fnadd WITH pp
     ON KEY LABEL del do borra with varread()
     ON KEY LABEL ins do anexa with varread()
     RETURN .T.
ENDIF
opc = acepta( ;
      'Desea Insertar una Nueva Cuota' ;
      )
IF opc = 2
     opc = acepta( ;
           'Desea Insertar un Nuevo detalle' ;
           )
     IF opc = 2
          ON KEY LABEL del do borra with;
varread()
          ON KEY LABEL ins do anexa with;
varread()
          RETURN .T.
     ENDIF
     pp=&npicked
     IF pp = 1
          = AINS(datos, pp + 1)
          pp = pp + 1
     ELSE
          = AINS(datos, pp + 1)
          pp = pp + 1
     ENDIF
     x = x + 1
     mdatos = datos(pp)
     mcuota = SPACE(7)
     mfec = SPACE(12)
     mdesc = SPACE(35)
     mmon = SPACE(15)
     mcon = SPACE(2)
     mdatos = mcuota + '³' + mcon +  ;
              ' ' + mdesc + '³' +  ;
              mfec + '³' + mmon +  ;
              '³'
     datos( pp) = mdatos
     SELECT 3
     xx = 1
     FOR ii = 1 TO x
          mdatos = datos(ii)
          IF VAL(LEFT(mdatos, 7)) <>  ;
             0
               mfechav = DTOC(GOMONTH(mfecha_in,  ;
                         xx))
               mdatos = STUFF(mdatos,  ;
                        49, 10,  ;
                        mfechav)
               mdatos = STUFF(mdatos,  ;
                        1, 7,  ;
                        STR(xx,  ;
                        7))
               datos( ii) =  ;
                    mdatos
               xx = xx + 1
          ENDIF
     ENDFOR
     SELECT 2
     SHOW GET m.da
     SHOW GETS
     suma = 0.00 
     FOR i = 1 TO x
          suma = suma +  ;
                 VAL(SUBSTR(datos(i),  ;
                 63, 13))
     ENDFOR
     SELECT 2
     @ 21, 63 GET suma PICTURE  ;
       '99,999,999.99' COLOR ,W+/ ;
       W 
     CLEAR GETS
     SHOW GET m.da
     DO fnadd WITH pp
     ON KEY LABEL del do borra with varread()
     ON KEY LABEL ins do anexa with varread()
     RETURN .T.
ENDIF
SELECT 3
mnrocuotas = mnrocuotas + 1
IF mnrocuotas > 6
     = msgerro( ;
       'El Maximo de Cuotas por Plan de Cobro deben ser 6, verifique' ;
       )
     ON KEY LABEL del do borra with varread()
     ON KEY LABEL ins do anexa with varread()
     RETURN .T.
ENDIF
mfecha_fin = GOMONTH(mfecha_in -  ;
             1, mnrocuotas)
@ 02, 16 SAY mnrocuotas PICTURE  ;
  '99' COLOR W+/W 
pp=&npicked
pp = pp + 1
FOR kk = pp TO x
     mdatos = datos(kk)
     IF VAL(LEFT(mdatos, 7)) <> 0
          EXIT
     ENDIF
ENDFOR
pp = kk - 1
x = x + 1
= AINS(datos, pp + 1)
pp = pp + 1
mdatos = datos(pp)
mcuota = STR(99, 7)
mfec = SPACE(12)
mdesc = SPACE(35)
mmon = SPACE(15)
mcon = SPACE(2)
mdatos = mcuota + '³' + mcon +  ;
         ' ' + mdesc + '³' + mfec +  ;
         '³' + mmon + '³'
datos( pp) = mdatos
xx = 1
FOR ii = 1 TO x
     mdatos = datos(ii)
     IF VAL(LEFT(mdatos, 7)) <> 0
          mfechav = DTOC(GOMONTH(mfecha_in,  ;
                    xx))
          mdatos = STUFF(mdatos,  ;
                   49, 10,  ;
                   mfechav)
          mdatos = STUFF(mdatos,  ;
                   1, 7, STR(xx,  ;
                   7))
          datos( ii) = mdatos
          xx = xx + 1
     ENDIF
ENDFOR
SHOW GET m.da
SHOW GETS
suma = 0.00 
FOR i = 1 TO x
     suma = suma +  ;
            VAL(SUBSTR(datos(i),  ;
            63, 13))
ENDFOR
SELECT 2
@ 21, 63 GET suma PICTURE  ;
  '99,999,999.99' COLOR ,W+/W 
CLEAR GETS
SHOW GET m.da
DO fnadd WITH pp
ON KEY LABEL del do borra with varread()
ON KEY LABEL ins do anexa with varread()
RETURN .T.
*
FUNCTION slist_show
IF x > 0
     SHOW GET navailpos ENABLE
ELSE
     SHOW GET navailpos DISABLE
ENDIF
RETURN .T.
*
FUNCTION detalle
PARAMETER xcod
SELECT 1
IF  .NOT. SEEK(xcod)
     xcod = boxfield('CONCEPTOS', ;
            'CODIGO')
     xdes = SUBSTR(descripcio, 1,  ;
            35)
ENDIF
SELECT 1
SEEK xcod
xdes = SUBSTR(descripcio, 1, 35)
xcod = codigo
mcon = xcod
mdes = xdes
@ 20, 08 SAY mcon PICTURE '@!'  ;
  COLOR W+/W 
@ 20, 11 SAY mdes PICTURE '@!'  ;
  COLOR W+/W 
SHOW GET mcon
RETURN .T.
*
PROCEDURE validar
PARAMETER xvar
DO CASE
     CASE xvar = 'MCODIGO'
          ON KEY LABEL tab
          SET FILTER TO lapmat = lapso
          mcodigo = boxfield('PLANCOBR', ;
                    'CODIGO')
          SHOW GET mcodigo
ENDCASE
ON KEY LABEL tab do validar with varread()
RETURN
*
