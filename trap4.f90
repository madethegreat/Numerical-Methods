program trapezoid
implicit none
real(8)::h,fi=0.0,y,x=0.0,func,z=0.0,a,b,f2=0.0,f3,f4=0.0
integer::i,j,k,n

n=1
a=0.d0
!b=1.d0
b=10.d0
!b=1.d0
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
f3=f3*2.0/sqrt((4*atan(1d0)))
print*,n,f3
 if(abs(f3-f4)<1e-10) exit



end do
print*,erf(b)


end program trapezoid

real(8) function func(x)
real(8),intent(in)::x
func=exp(-x**2)
end function func


