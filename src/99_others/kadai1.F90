program kadai1
    implicit none
    integer                 :: i, nd
    integer                 :: lr1, lr2
    integer                 :: like_num, seedsize, c, ir, jr  ! like_num: 評価をつける回数, seedsize,c,ir,jr: rondom生成
    real(8)                 :: rnd ! rnd: random生成のための実数
    integer, allocatable    :: seed(:) ! seed: 乱数生成

    call system_clock(count=c) !時間を取得
    call random_seed(size=seedsize)
    allocate(seed(seedsize))
    call random_seed(get=seed)
    seed = c !時間を全部に代入
    call random_number(rnd)
    like_num = int(rnd) * 10.d0 + 20.d0


    lr1 = 10
    open(lr1, file='item.dat')
    open(lr2, file='data.dat')
    do
        read(lr1, *, end=999)
        nd = nd + 1
    enddo
    999 continue
    rewind(999)
    
    do i=1,like_num
        call random_number(rnd)
        ir=int(rnd * nd)
    enddo

    do i = 1, nd
        read(lr1, *) mvID(:)
        do j = 1, like_num
            if (i .eq. j) then
                call random_number(rnd)
                jr=int(rnd * 10.d0) / 2
                write(lr2, *)jr
            else
                write(lr2, *) 0.d0
        enddo
    enddo
    
end program kadai1