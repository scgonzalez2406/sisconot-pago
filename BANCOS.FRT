   Q   !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              EPSON                          
BANCOS.DBF                     BANCOS                         CODIGO                         DATE()                         bancos.codigo                  bancos.descripcio              _pageno                        "999"                          J                              l_nombre                       "@A"                           IB                             	"FECHA :"                      
"PAGINA :"                     "CONTROL DE PAGOS"             B                              	"C�digo "                      CIB                            ("LISTADOS GENERAL DE BANCOS COMERCIALES"                        B                              "Nombres    "                  "Tipo de Cuenta"               "Nro. Cuenta"                  3IIF(bancos.tipocuenta $ "Ah","Ahorros","Corriente")             ALLTRIM(bancos.nrocuenta)      EPSON                          CONCEPTO.DBF                   CONCEPTO                       CODIGO                         DATE()                         concepto.codigo                concepto.descripcio            _pageno                        J                              "999"                          l_nombre                       IB                             "@A"                           	"FECHA :"                      
"PAGINA :"                     ("LISTADOS GENERAL DE CONCEPTO DE COBROS"                        CIB                            "CONTROL DE PAGOS"             	"C�digo "                      B                              "Descripci�n"                  B                             �* WIZARD = QREPORT
* WIZNAME = C:\FPD26\WIZARDS\WZ_QREPO.APP
* WIZCLASS = REPORT
* VERSION = 1.0
* TIMESTAMP = 549997369.00
* STYLE = EXECUTIVE
* LAYOUT = COLUMNAR
* TITLE = Asignatu
* COLUMNS = 1.00
* TABLE = "C:\YOEL�\ASIGNATU.DBF"
* FIELDS = Codigo;Nombre;Corto;
* SORTS = ;
* GROUPS = ;
* MODIFIERS = ;
* TOTALS = 0.00
* SUBTOTALS = 0.00
* TOPBOTTOM = 1.00
* LABELS = ;
* AVERY = 1.00
* TAGASCEND = 1.00
