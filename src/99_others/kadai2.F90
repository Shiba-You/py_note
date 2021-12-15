program kadai2
    implicit none
    integer                 :: i, nd
    real(8), allocatable    :: nm(:), userID(:)
    integer                 :: lr2

    lr2 = 10
    open(lr2, file='data.dat')
    do
        read(lr2, *, end=999)
        nd = nd + 1
    enddo
    999 continue
    rewind(999)

    do i = 1, nd
        read(lr2, *) userID(:)
        do j = 1, like_num
            if (i .eq. j) then
                call random_number(rnd)
                jr=int(rnd * 10.d0) / 2
                write(lr2, *)jr
            else
                write(lr2, *) 0.d0
        enddo
    enddo
    
end program kadai2