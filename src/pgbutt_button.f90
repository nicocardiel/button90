module pgbutt_button
    implicit none

    type :: SingleButton
        integer :: num = 0
        real :: x1, x2, y1, y2
        real :: ddx, ddy
        real :: xgap, ygap
        real :: offy
        character(len=:), allocatable :: text
        integer :: mode = -1
        integer :: pgscf = 2
        real :: pgsch = 1.0
        real :: ytext = 0.35
        logical :: exist = .false.
    end type SingleButton

end module pgbutt_button
