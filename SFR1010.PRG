* Inicializar entorno para la ventana
CLOSE DATABASES
SET TALK OFF
CLEAR GETS

* Definir la ventana
DEFINE WINDOW pagos0 FROM 00, 00  ;
   TO 24, 79 GROW FLOAT CLOSE  ;
   ZOOM SHADOW TITLE  ;
   'Control de Pagos enviados (SFR1010)'  ;
   MINIMIZE SYSTEM COLOR  ;
   SCHEME 5
MOVE WINDOW pagos0 CENTER
ACTIVATE WINDOW pagos0

@ 00, 00 say 'ýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýýý{'

flag = .T.

DO WHILE flag

	tecla = 1
	DO WHILE tecla = 1
		tecla = INKEY()
	ENDDO

	IF LASTKEY() = 27
		flag = .F.
		DO lib\Window
		RELEASE WINDOW pagos0
	ENDIF

ENDDO