program root
implicit none
real*8::x, f, b, b1, b2, c, d, h,a
integer::i,j

a=0
b=10

d = f(b)
h = 0.005
do i = 0, 1000
b = a+i*h
 c = f(b)
 d = f(b+h)
  
if (c*d .lt. 0) then
b1 = b + h
b2 = b
 do j = 1, 1000
 if (f((b1+b2)/2)*f(b1) .lt. 0) then
  b2 = (b1+b2)/2
  else
b1 = (b1+b2)/2
 end if
 if (abs(b2-b1)/b1 .lt. 1e-12) exit 
 end do
 print*, i, b1, f(b1)
  end if     
 end do
 end program root
 
 real*8 function f(x)
 implicit none
 real*8::x
 f=x**6-9.2*x**5+34.45*x**4-66.914*x**3+70.684*x**2-38.168*x+8.112+0.01234*log(x)
 end function
