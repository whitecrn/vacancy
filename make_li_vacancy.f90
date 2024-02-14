module list
        implicit none
        type :: atom
                integer :: element
                real :: x(3)
        end type atom

contains

!subroutine bubble_sort(C,N)
!        implicit none
!        integer :: N
!        integer:: C(:)
!        integer :: iii,jjj,temp
!        do iii=1,N-1
!                do jjj=1,N-iii
!                        if (C(jjj) < C(jjj+1)) then
!                                temp = C(jjj)
!                                C(jjj) = C(jjj+1)
!                                C(jjj+1) = temp
!                        end if
!                end do
!        end do
!        return
!end subroutine bubble_sort

subroutine wrong(r,s,C)
        implicit none
        logical :: s
        real :: r(:)
        integer :: s_size,ii,jj,C
        s_size = size(r)
loop1:  do ii=1,s_size-1
loop2:          do jj=ii+1,s_size
                        if (r(ii) == r(jj)) then
                                s = .true.
                                C = jj
                                return
                        else if (r(ii) /= r(jj)) then
                                s = .false.
                        end if
                end do loop2
        end do loop1
end subroutine wrong

end module list

program vacancy
        use list
        implicit none
        character(len=20) :: vacancy_file,file_num
        real :: factor
        real :: xx,yy,zz,xy,xz,yx,yz,zx,zy
        integer :: i,j,k,g,gg,l
        integer :: n_li,n_la,n_zr,n_o,n,op
        real,allocatable :: r(:)
        integer,allocatable :: r_i(:)
        logical :: alive=.false.
        logical :: s=.false.
        type(atom),allocatable :: A(:),B(:)
        integer :: v,v_step,temp,C

        op = 1
        inquire(file='CONTCAR',exist=alive)
        if (alive) then
                open(unit=100,file='CONTCAR',status='old',action='read')
                read(100,*)
                read(100,*) factor
                read(100,*) xx,xy,xz
                read(100,*) yx,yy,yz
                read(100,*) zx,zy,zz
                read(100,*)
                read(100,*) n_li,n_la,n_zr,n_o
                n = n_li+n_la+n_zr+n_o
                read(100,*) 
                allocate(A(n))
                do i=1,n
                        read(100,*) A(i)%x(1),A(i)%x(2),A(i)%x(3)
                        if (i>=1 .and. i<=n_li) then
                                A(i)%element=1
                        else if (i>n_li .and. i<=n_li+n_la) then
                                A(i)%element=2
                        else if (i>n_li+n_la .and. i<=n_li+n_la+n_zr) then
                                A(i)%element=3
                        else if (i>n_li+n_la+n_zr .and. i<=n) then
                                A(i)%element=4
                        end if
                end do
                if (op == 1) then
                        v = int(floor(0.1*n_li))
                else if (op == 2) then
                        v = int(floor(0.1*n_la))
                else if (op == 3) then
                        v = int(floor(0.1*n_zr))
                else if (op == 4) then
                        v = int(floor(0.1*n_o))
                end if 
                v_step = int(floor(v*0.1))
                do i=1,v,v_step !to vacancies
                        allocate(B(n-i))
                        allocate(r_i(i))
56                      allocate(r(i))
                        do j=1,i !to every single vacancy, create a random number
                                call random_seed
                                call random_number(r(j))
                                if (op == 1) then
                                        r(j) = 1+int(floor(r(j)*n_li))
                                else if (op == 2) then
                                        r(j) = n_li+1+int(floor(r(j)*n_la))
                                else if (op == 3) then
                                        r(j) = n_li+n_la+1+int(floor(r(j)*n_zr))
                                else if (op == 4) then
                                        r(j) = n_li+n_la+n_zr+1+int(floor(r(j)*n_o))
                                end if
                        end do
55                      call wrong(r,s,C)
                        if (s) then
                                if (C /= n_li .or. C /= n_li+n_la .or. C /= n_li+n_la+n_zr .or. C /= n) then
                                        r(C) = r(C)+1
                                        goto 55
                                else 
                                        r(C) = r(C)-1
                                        goto 55
                                end if
                        end if
                        if (i /= 1) then
                                do j=i-1,1,-1
                                        do k=1,j
                                                if (r(k) < r(k+1)) then
                                                temp = r(k)
                                                r(k) = r(k+1)
                                                r(k+1) = temp
                                                end if
                                        end do
                                end do
                        end if        
                        !call bubble_sort(int(r(:)),gg)
                        r_i(:) = int(r(:))
                        !do j=1,gg !to every vacancy
                                !do k=0,n-r(j) !to delete atom
                                !        B(r(j)+k) = B(r(j)+k+1)
                                !end do
                        !end do
                        k = 1
                        do j=1,n
                                if (any(r_i == j)) then
                                        cycle
                                else
                                        B(k) = A(j)
                                        k = k+1
                                end if
                        end do
                        write(file_num,"(I4)") i
                        vacancy_file = 'POSCAR-' // trim(adjustl(file_num))
                        open(unit=200,file=vacancy_file,status='replace')
                        write(200,*) "llzo"
                        write(200,"(F3.1)")  factor
                        write(200,*) xx,xy,xz
                        write(200,*) yx,yy,yz
                        write(200,*) zx,zy,zz
                        write(200,*) "Li  La  Zr  O"
                        if (op == 1) then
                                write(200,*) n_li-i,n_la,n_zr,n_o
                        else if (op == 2) then
                                write(200,*) n_li,n_la-i,n_zr,n_o
                        else if (op == 3) then
                                write(200,*) n_li,n_la,n_zr-i,n_o
                        else if (op == 4) then
                                write(200,*) n_li,n_la,n_zr,n_o-i
                        end if
                        write(200,*) "Cartesian"
                        
                        do j=1,n-i
                                write(200,"(3(F15.12,2X))") B(j)%x(1),B(j)%x(2),B(j)%x(3)
                        end do
                        deallocate(B)
                        deallocate(r)
                        deallocate(r_i)
              end do 
        else 
                stop
        end if
stop
end program
