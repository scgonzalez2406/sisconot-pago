close data
set talk off
set dele on
select 1
use contdeud
copy to contd
select 2
use contd exclu
dele all for lapsomat=space(6)
pack
index on lapsomat tag lapsomat
use
select 2
use contd order lapsomat
select 2
go top
lapmat=" "
m.deudamp=0.00
m.deudaad=0.00
x=0
y=0
z=0
do while .not. eof()
   x=x+1
   scatter memvar memo
   xced=m.cedula
   m.deuda=m.total_d-m.monto_p
   m.debe=m.total_d
   m.paga=m.monto_p
   lapmat=m.lapsomat
   select 3
   use mp&lapmat order ced
   if seek(xced)
      m.deudamp=total_deud-monto_paga
   else
      m.deudamp=0.00
   endif
   select 4
   use ad&lapmat order ced
   if seek(xced)
      m.deudaad=total_deud-monto_paga
   else
      m.deudaad=0.00
   endif
   m.total_d=m.deudamp+m.deudaad
   if m.total_d>0.00
      m.monto_p=0.00
   else
      m.monto_p=m.paga
   endif
   m.monto1=m.total_d-m.monto_p
   m.monto2=0.00
   m.monto3=0.00
   m.monto4=0.00
   m.monto5=0.00
   select 3
   use
   select 4
   use
   @10,10 say x
   select 2
   if m.total_d>=m.debe
      m.revisado="S"
      y=y+1
      @11,10 say y
      if m.total_d>m.debe .and. m.debe<>0.00
         z=z+1
         @12,10 say z
         m.mayor="S"
      endif
      if m.debe<>0.00
         gather memvar memo
      endif
   endif
   skip
   m.deudamp=0.00
   m.deudaad=0.00
enddo
close data
