program samplebutton
    use pgbutt_modules
    implicit none

    type(CanvasDef) :: canvas
    type(Viewport), pointer :: viewport1, viewport2
    type(Viewport), pointer :: ptr
    integer :: i
    integer :: nb
    real :: xc, yc
    real :: xx(100), yy(100)
    character(len=1) ch
    logical :: verbose = .false.

    ! open graphics output
    call rpgbegin(verbose)

    ! define viewports for buttons and plots
    call canvas%add_button_viewport(0.05, 0.95, 0.80, 0.95, 6, 2, viewport1)
    call canvas%add_plot_viewport(0.10, 0.95, 0.10, 0.80, viewport2)

    ! define buttons in selected viewport
    call viewport1%button(1, 'sin', 0)
    call viewport1%button(2, 'cos', 0)
    call viewport1%button(6, 'EXIT', 0)

    ! main loop
    do
        call pgcurs(xc, yc, ch)
        call canvas%get_button_from_mouse(xc, yc, ptr, nb, verbose)
        
        if (associated(ptr, viewport1)) then
            if (nb.eq.1) then
                call ptr%button(nb, 'sin', 5)
                do i = 1, 100
                    xx(i) = real(i-1)/99
                    yy(i) = sin(xx(i) * 2 * 3.141593)
                end do
                call rpgenv(viewport2, 0., 1., -1.1, 1.1, 0, 0)
                call pgline(100, xx, yy)
                call pgvport(0.,1.,0.,1.)
                call pgwindow(0.,1.,0.,1.)
                call ptr%button(nb, 'sin', 0)
            elseif (nb.eq.2) then
                call ptr%button(nb, 'cos', 5)
                do i = 1, 100
                    xx(i) = real(i-1)/99
                    yy(i) = cos(xx(i) * 2 * 3.141593)
                end do
                call rpgenv(viewport2, 0., 1., -1.1, 1.1, 0, 0)
                call pgline(100, xx, yy)
                call pgvport(0.,1.,0.,1.)
                call pgwindow(0.,1.,0.,1.)
                call viewport1%button(nb, 'cos', 0)
            elseif (nb.eq.6) then
                call viewport1%button(6, 'EXIT', 5)
                write(*, '(a,$)') 'Press <RETURN> to EXIT'
                read(*, *)
                exit
            end if
        elseif (associated(ptr, viewport2)) then
            print *, 'viewport2, xc, yc, nb:', xc, yc, nb
        else
            write(*, '(a,$)') 'Outside viewports! Cursor at:'
            write(*, *) xc, yc
        end if
    end do

    !end of program
    call pgend

end program samplebutton
