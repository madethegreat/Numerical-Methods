program daft2
real(8)::x, f, t, g, a
complex(16)::i, gg(0:128),gi(0:1280)
real(8), parameter:: pi = 4d0*atan(1d0)
integer:: j, k, l,N	
	
a = 16
N = 64
gg = 0
i = (0.0, 1d0)

do j = 0, N-1
do k = 0, N-1
gg(j) = gg(j) + g(k)*exp(2*pi*i*j*k/N)
end do
end do

open(file="daft",unit=11)
do j = N/2, N-1
write(11,*)j-N, gg(j),abs(gg(j))**2.0
end do
do j = 0, N/2
write(11,*)j, gg(j),abs(gg(j))**2.0
end do

gi=0.0

open(file="newday",unit=11)
do k=0,N*10
t=-a+(2*a*k/(N*10))
do j=0,N/2-1
gi(k)=gi(k)+gg(j)*exp(-2.d0*pi*i*j*((t+a)/(2.d0*a)))/N
end do
do j=N/2,N-1
gi(k)=gi(k)+gg(j)*exp(-2.0d0*pi*i*(j-N)*((t+a)/(2.d0*a)))/N
end do
write(11,'(4e13.5)')t,gi(k)+((a-abs(t)**2.d0)/a)-(exp(-2.d0*a)*t**2.0/a),exp(-2.0*abs(t))
end do
close(11)



end program daft2

real(8) function g(k)
implicit none
integer::n,k
real(8)::pi,a,t
n=64
a=16.d0
t=-a+(2*a*k/(n))
pi=4.d0*atan(1.d0)
!!?? error in the unction, should be (a-abs(t))**2
g=exp(-2*abs(t))-1.d0*(a-abs(t)**2.d0)/a +((1.d0/a)*exp(-2.d0*a)*(t)**2)
end function g











