program trapezoid
implicit none
real(8)::h,fi=0.0,y,x=0.0,func,z=0.0,a,b,f2=0.0,f3=0.0,f4=0.0
integer::i,j,k,n,l
!!?? use 0.2d0 etc.
real,dimension(4)::ar=(/0.2,0.1,0.01,0.002/)
real::f(4)
f=0
a=ar(1)
b=1d0
do k=1,4
n=1
print*,a,b
f2=(1.0/2.0)*(func(a)+func(b))
fi=0.0
f4=0.0
f3=1
!!?? 50 is too much, can't go beyond 30
do j=1,40
f4=f3
n=n*2
h=(b-a)/n
do i=1,n-1,2
fi=fi+func(a+i*h)
end do
f3=(f2+fi)*h
print*,n,f3

 if(abs(f3-f4)<1e-10)then
         !!?? index l doesn't appear in the loop and you are not printing the result
         !!?? what if it doesn't converg, use only exit and put this after
         !!?? enddo so that it is done irrespective of convergence.
         !!?? no need of loop, just go on adding integral to anothe variable
 do l=k,4
 f(k)=f3+f(k) 
 end do 
 exit
end if 
end do

a=ar(k+1)
b=ar(k)


end do


end program trapezoid

real(8) function func(x)
real(8),intent(in)::x
func=exp(1.0/x)
end function func


