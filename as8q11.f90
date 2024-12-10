program as8
real*8::x, y, a(4), f, b, b1, b2, c, d, h
integer:: i, j, k



a=(/2.0, 10.0, 20.0, 20.3959/)

do k = 1, 4
 b = -25.1
y = a(k)
d = f(b,y)
h = 0.002
print*, 'y =', y
do i = 1, 25000
b = b + h
 c = f(b,y)
 
if (c*d .lt. 0) then
 b1 = b-h
 b2 = b
 !!?? 10 bisections can only improve accuracy by 1d-3
 do j = 1, 10
 !!?? there is a compilation error here should be (b1+b2)/2
 if (f((b1+b2,y)/2)*f(b1,y) .lt. 0) then
 b2 = (b1+b2)/2
 else
 b1 = (b1+b2)/2
 end if
 if (abs(b2-b1) .lt. 1e-10) exit 
 end do
 print*, b1, f(b1,y)
 end if     
 d = c
  end do
end do
end program as8

real*8 function f(x,y)
implicit none
real*8 ::x,y
f=x-y*sin(x)
end function f
