program samplebutton
    use pgbutt_modules
    implicit none

    type(CanvasDef) :: canvas
    type(Viewport), pointer :: ptr1, ptr2
    type(Viewport), pointer :: ptr
    integer :: i
    integer :: nb
    real :: xc, yc
    real :: xworld, yworld
    real :: xx(100), yy(100)
    character(len=1) ch
    logical :: verbose = .false.

    ! open graphics output
    call rpgbegin(verbose)

    ! define viewports for buttons and plots
    call canvas%add_button_viewport(0.05, 0.95, 0.80, 0.95, 6, 2, ptr1)
    call canvas%add_plot_viewport(0.10, 0.95, 0.10, 0.70, ptr2)

    ! define buttons in selected viewport
    call ptr1%button(1, 'sin', 0)
    call ptr1%button(2, 'cos', 0)
    call ptr1%button(3, 'clear', 0)
    call ptr1%button(4, 'color', 0)
    call ptr1%button(6, 'EXIT', 0)
    call ptr1%button(7, 'mode 0', 0)
    call ptr1%button(8, 'mode 1', 0)
    call ptr1%button(8, 'mode 1', 1)
    call ptr1%button(9, 'mode 2', 0)
    call ptr1%button(10, 'mode 3', 0)
    call ptr1%button(10, 'mode 3', 3)
    call ptr1%button(11, 'mode 4', 4)
    call ptr1%button(12, 'mode 5', 5)

    ! main loop
    do
        call canvas%get_button_from_mouse(xc, yc, ch, ptr, nb, verbose)
        
        if (associated(ptr, ptr1)) then
            if (nb.eq.1) then
                call ptr%button(nb, 'sin', 5)
                do i = 1, 100
                    xx(i) = real(i-1)/99
                    yy(i) = sin(xx(i) * 2 * 3.141593)
                end do
                call rpgenv(ptr2, 0., 1., -1.1, 1.1, 0, 0)
                call rpgline(ptr2, 100, xx, yy)
                call ptr%button(nb, 'sin', 0)
            elseif (nb.eq.2) then
                call ptr%button(nb, 'cos', 5)
                do i = 1, 100
                    xx(i) = real(i-1)/99
                    yy(i) = cos(xx(i) * 2 * 3.141593)
                end do
                call rpgenv(ptr2, 0., 1., -1.1, 1.1, 0, 0)
                call rpgline(ptr2, 100, xx, yy)
                call ptr1%button(nb, 'cos', 0)
            elseif (nb.eq.3) then
                call ptr%button(nb, 'clear', 5)
                call rpgerasw(ptr2)
                call ptr%button(nb, 'clear', 0)
            elseif (nb.eq.6) then
                call ptr1%button(6, 'EXIT', 5)
                write(*, '(a,$)') 'Press <RETURN> to EXIT'
                read(*, *)
                exit
            end if
        elseif (associated(ptr, ptr2)) then
            call ptr2%world(xc, yc, xworld, yworld)
            write(*, '(a,$)') 'Cursor at:'
            write(*, *) xworld, yworld
        end if
    end do

    !end of program
    call pgend

end program samplebutton
