module gomoku_m
    integer,parameter               :: Win=5,N=15
    type gomoku
        integer						:: chess(N,N)
		integer						:: n_zeros=N*N
		integer,pointer				:: zeros(:,:)
    end type
    
	contains
	
	subroutine down(g,x,y,w)
		implicit none
		type(gomoku)				:: g
		integer,intent(in)			:: x,y,w

		if(g%chess(x,y)==0) then
			g%chess(x,y)=w
			g%n_zeros=g%n_zeros-1
		elseif(w==0) then
			g%chess(x,y)=0
			g%n_zeros=g%n_zeros+1
		else
			print '(I2," already in(",I1,",",I2,")")',g%chess(x,y),x,y
		end if
	end subroutine

	subroutine show(g)
		implicit none
		type(gomoku)				:: g
		integer						:: i
		
		print '(2x,'//char(N/10+48)//char(mod(N,10)+48)//'I2)',[(mod(i,10),i=1,N)]
		do i = 1,N
			print '(20I2)',mod(i,10),g%chess(i,:)
		end do
	end subroutine
	
	function judge(g,x,y)	result(w)
		implicit none
		type(gomoku)				:: g
		integer,intent(in)			:: x,y
		integer						:: i,w,t,l(4)
		integer,save				:: D(4,2)=(/0,-1,-1,-1,-1,-1,0,1/) !°´ÁĞ¶ÁÈ¡
		t=g%chess(x,y)
		w=0
		if(t==0) return
		do i = 1,4
			l(i)=length(g%chess,x,y,D(i,1),D(i,2),t) &
					+ length(g%chess,x,y,-D(i,1),-D(i,2),t)+1
			if(l(i)>=Win) then
				w=t
				return
			end if
		end do

		contains
		function length(m,x,y,dx,dy,w) result(l)
			integer,intent(in)		:: x,y,dx,dy,w,m(:,:)
			integer					:: i,j,l
			l=0
			i=x+dx
			j=y+dy
			do while(m(i,j)==w .and. i>0 .and. j>0 .and. i<=N .and. j<=N)
				l=l+1
				i=i+dx
				j=j+dy
			end do
		end function
	end function
	
	subroutine getzeros(g,r)
		implicit none
		type(gomoku),intent(in)			:: g
		integer,pointer					:: r(:,:)
		integer 						:: i,j,t
		t=1
		allocate(r(g%n_zeros,2))
		do i = 1,N
			do j =1,N
				if(g%chess(i,j)==0) then
					r(t,:)=(/i,j/)
					t=t+1
				end if
			end do
		end do
		if(t/=g%n_zeros+1) pause "getzeros ValueError"
	end subroutine

	function randint(l,u) result(i)
		implicit none
		integer,intent(in)			:: l,u
		integer						:: i
		real						:: temp	
		call random_number(temp)
		i=int(temp*(u-l+1))+l
	end function
	
end module

program main
	use gomoku_m
	implicit none
	type(gomoku)					:: a
	integer							:: i,j,k,temp(10),w
	integer,pointer					:: r(:,:)
	print *,"hello,world!",Win,N
	call random_seed() 
	w=1
	do 
		i=randint(1,a%n_zeros)
		call getzeros(a,r)
		call down(a,r(i,1),r(i,2),w)
		call show(a)
		j=judge(a,r(i,1),r(i,2))
		if(j/=0) exit
		w=-w
	end do
	print *,j

end program