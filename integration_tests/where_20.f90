! A vector subscript in a where construct
program where_20
    implicit none
    integer :: idx(4), table(5), out(4)
    integer :: idx2(4,2), table2(5,2), out2(4,2)
    integer :: k

    table = [10, 20, 30, 40, 50]
    idx = [2, 7, 5, 0]

    ! in the assigned value
    out = 0
    where (idx > 0 .and. idx <= 5)
        out = table(idx)
    elsewhere
        out = -1
    end where
    if (any(out /= [20, -1, 50, -1])) error stop

    ! in the mask
    idx = [2, 1, 5, 4]
    out = 0
    where (table(idx) > 15)
        out = 1
    elsewhere
        out = 2
    end where
    if (any(out /= [1, 2, 1, 1])) error stop
    idx = [2, 7, 5, 0]

    ! an array of rank two, the subscript of a section
    table2(:,1) = table
    table2(:,2) = table + 100
    idx2(:,1) = idx
    idx2(:,2) = [1, 3, 9, 4]
    do k = 1, 2
        where (0 < idx2(:, k) .and. idx2(:, k) <= 5)
            out2(:, k) = table2(idx2(:, k), k)
        elsewhere
            out2(:, k) = -1
        end where
    end do
    if (any(out2(:,1) /= [20, -1, 50, -1])) error stop
    if (any(out2(:,2) /= [110, 130, -1, 140])) error stop
end program
