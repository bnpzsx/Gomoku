module app
	use gomoku_m
	contains
	
	function randdown(g,w,r) result(j)
		implicit none
		type(gomoku)				:: g
		integer,intent(in)			:: w
		integer						:: r(:,:),i,j
		
		i=randint(1,g%n_zeros)
		call down(g,r(i,1),r(i,2),w)
		j=judge(g,r(i,1),r(i,2))
		r(i:g%n_zeros,:)=r(i+1:g%n_zeros+1,:)
	end function
	
	function multidown(g,r,w0,t) result(p)
		implicit none
		type(gomoku),intent(in)		:: g
		integer,intent(in)			:: w0,t,r(N*N,2)
		real(8)						:: p
		type(gomoku)				:: a
		integer						:: i,j,w,wt,s(N*N,2)
		wt=0
		do i = 1,t
			a=g
			s=r
			j=0
			w=w0
			do while(j==0)
				j=randdown(a,w,s)
				w=-w
			end do
			wt=wt+j
		end do
		p=dble(wt)/dble(t)
	end function


end module

program main
	use app
	implicit none
	type(gomoku)					:: a,b
	integer							:: j,w,k
	integer							:: r(N*N,2)
	real::t_derivs=0.,start,finish
	print *,"Hello,world"
	call cpu_time(start)
	call random_seed() 
	call down(a,8,8,1)
	call down(a,8,9,1)
	call down(a,8,10,1)
	call down(a,8,11,1)
	call getzeros(a,r)
	print *,multidown(a,r,1,100000)
	call cpu_time(finish)
	print *,finish-start
	pause "Press any key to continue"
end program