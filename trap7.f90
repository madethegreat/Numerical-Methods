program trapezoid
implicit none
real(8)::h,fi=0.0,y,x=0.0,func,z=0.0,a,b,f2=0.0,f3,f4=0.0
integer::i,j,k,n

n=1
!!?? use 0.1d0 etc
a=0.1
b=0.2
f2=1.0/2.0*(func(a)+func(b))
fi=0.0
f4=0.0
f3=1
do j=1,25
f4=f3
n=n*2
h=(b-a)/n
do i=1,n-1,2
fi=fi+func(a+i*h)
end do
f3=(f2+fi)*h
print*,n,f3
 if(abs(f3-f4)<1e-10) exit



end do


end program trapezoid

real(8) function func(x)
real(8),intent(in)::x
real(8)::pi,res=0.0
integer::j
pi=4*atan(1.d0)
res=0.0
do j=1,50
res=res+(((2d0**(-j))*cos((7d0**j)*pi*x)))
end do
res=res/pi
func=res
end function func


