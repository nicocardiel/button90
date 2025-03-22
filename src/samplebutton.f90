program samplebutton
    use pgbutt_modules
    implicit none

    type(CanvasDef) :: canvas
    type(Viewport), pointer :: viewport1, viewport2
    type(Viewport), pointer :: viewport_ptr
    integer :: i
    integer :: nb
    real :: xc, yc
    real :: xx(100), yy(100)
    character(len=1) ch
    logical :: verbose = .true.

    !open graphics output
    call rpgbegin(verbose_=verbose)

    call canvas%add_button_viewport(0.05, 0.95, 0.80, 0.95, 6, 2, viewport1)
    call canvas%add_plot_viewport(0.10, 0.95, 0.10, 0.70, viewport2)

    call viewport1%button(1, 'sin', 0)
    call viewport1%button(2, 'cos', 0)
    call viewport1%button(6, 'EXIT', 0)

    ! main loop
    do
        call pgcurs(xc, yc, ch)
        call canvas%get_viewport_from_mouse(xc, yc, .true., viewport_ptr)
        if (associated(viewport_ptr)) then
            print *, 'Found #', viewport_ptr%number
            print *, associated(viewport_ptr, viewport1)
            print *, associated(viewport_ptr, viewport2)
            call viewport_ptr%ifbutton(xc, yc, nb)
            print *, 'nb=', nb
        end if
        
        !---
        if (nb.eq.0) then
            write(*, '(a,$)') 'Cursor at:'
            write(*, *) xc, yc
        !---
        elseif (nb.eq.1) then
            call viewport1%button(nb, 'sin', 5)
            do i = 1, 100
                xx(i) = real(i-1)/99
                yy(i) = sin(xx(i) * 2 * 3.141593)
            end do
            call rpgenv(viewport2, 0., 1., -1.1, 1.1, 0, 0)
            call pgline(100, xx, yy)
            call pgvport(0.,1.,0.,1.)
            call pgwindow(0.,1.,0.,1.)
            call viewport1%button(nb, 'sin', 0)
        !---
        elseif (nb.eq.2) then
            call viewport1%button(nb, 'cos', 5)
            do i = 1, 100
                xx(i) = real(i-1)/99
                yy(i) = cos(xx(i) * 2 * 3.141593)
            end do
            call rpgenv(viewport2, 0., 1., -1.1, 1.1, 0, 0)
            call pgline(100, xx, yy)
            call pgvport(0.,1.,0.,1.)
            call pgwindow(0.,1.,0.,1.)
            call viewport1%button(nb, 'cos', 0)
        !---
        elseif (nb.eq.6) then
            call viewport1%button(6, 'EXIT', 5)
            write(*, '(a,$)') 'Press <RETURN> to EXIT'
            read(*, *)
            exit
        end if
    end do

    !end of program
    call pgend

end program samplebutton
