close data
select 1
use mp022001 order ced && actual
select 2
use contdeud order cedula && deuda
select 3
use mp012001 order ced && anterior
go top
do while .not. eof()
   xced=ced
   select 1
   if !seek(xced)
       select 2
       if seek(xced)
          repla monto_p with 0.00, monto1 with 0.00
       endif
   endif
   select 3
   skip
enddo
close data       
          