module allocate_save_01_m
    implicit none
contains
    subroutine run(first, x)
        logical, intent(in) :: first
        double precision, intent(out) :: x
        ! the allocation of a saved allocatable array lasts from one call to the next
        double precision, allocatable, save :: table(:,:)
        integer, allocatable :: counts(:)
        save :: counts
        if (first) then
            allocate(table(3, 2), counts(2))
            table = 1
            table(2, 2) = 7
            counts = 5
        end if
        if (.not. allocated(table) .or. .not. allocated(counts)) error stop "not allocated"
        if (size(table, 1) /= 3 .or. any(counts /= 5)) error stop "wrong shape or value"
        x = table(2, 2)
    end subroutine
end module

subroutine plain(first)
    logical, intent(in) :: first
    real, allocatable, save :: v(:)
    if (first) then
        allocate(v(4))
        v = 2
    end if
    if (.not. allocated(v)) error stop "plain: not allocated"
    if (sum(v) /= 8) error stop "plain: wrong value"
end subroutine

program allocate_save_01
    use allocate_save_01_m
    implicit none
    double precision :: x
    call run(.true., x)
    if (x /= 7) error stop
    call run(.false., x)
    if (x /= 7) error stop
    call plain(.true.)
    call plain(.false.)
    print *, "ok"
end program
