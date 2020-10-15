USE mp022016
COPY TO BA022016

*--- los de plan 01
repla all plancobro with "01",;
          total_deud with 62000,;         
          cuota5 with 15000;
          cuota6 with 15000;
   for alltrim(plancobro)="01" and ultimacuot<=2


*--- los de plan 02
repla all plancobro with "02",;
          total_deud with 58000,;
          cuota5 with 15000;
          cuota6 with 15000;
   for alltrim(plancobro)="01" and ultimacuot=3


*--- los de plan 03
repla all plancobro with "03",;
          total_deud with 54000,;
          cuota5 with 15000;
          cuota6 with 15000;
   for alltrim(plancobro)="01" and ultimacuot=4

*--- los de plan 04
repla all plancobro with "04",;
          total_deud with 45000,;
          cuota6 with 15000;
   for alltrim(plancobro)="01" and ultimacuot=5
   
*--- los de plan 05
*repla all plancobro with "05",;
 *         total_deud with 36000;
  * for alltrim(plancobro)="01" and ultimacuot=6
   
