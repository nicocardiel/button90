program samplebutton
    use pgbutt_rpgfunctions
    use pgbutt_canvas
    implicit none

    type(CanvasDef) :: canvas
    type(Viewport), pointer :: viewport1, viewport2, viewport3
    type(Viewport), pointer :: viewport_ptr
    integer :: i
    integer :: nb
    real :: xc, yc
    character(len=1) ch
    character(len=255) cdummy
    logical :: verbose = .true.

    !open graphics output
    call rpgbegin(verbose_=verbose)

    call canvas%add_button_viewport(0.00, 1.00, 0.90, 1.00, 6, 1, viewport1)
    call canvas%add_button_viewport(0.00, 1.00, 0.68, 0.88, 6, 2, viewport2)
    call canvas%add_plot_viewport(0.00, 1.00, 0.00, 0.65, viewport3)

    if (verbose) then
        print *, canvas%list_button_viewports%count
        print *, canvas%list_plot_viewports%count
        call pgsci(2)
        call viewport1%plot_boundary()
        call viewport1%print_corners()
        call pgsci(3)
        call viewport2%plot_boundary()
        call viewport2%print_corners()
        call pgsci(4)
        call viewport3%plot_boundary()
        call viewport3%print_corners()
        call pgsci(1)
    end if

    do i = 1, size(viewport1%button_array)
        write(cdummy, '(a5,i2.2)') 'test_', i
        call viewport1%button(i, trim(cdummy), 0)
    end do

    do i = 1, size(viewport2%button_array)
        write(cdummy, '(a5,i2.2)') 'test_', i
        call viewport2%button(i, trim(cdummy), 0)
    end do

    do
        call pgcurs(xc, yc, ch)
        call canvas%get_viewport_from_mouse(xc, yc, .true., viewport_ptr)
        if (associated(viewport_ptr)) then
            print *, 'Found #', viewport_ptr%number
            print *, associated(viewport_ptr, viewport1)
            print *, associated(viewport_ptr, viewport2)
            print *, associated(viewport_ptr, viewport3)
            call viewport_ptr%ifbutton(xc, yc, nb)
            print *, 'nb=', nb
        end if
        if (ch .eq. 'X') exit
    end do

    !end of program
    call pgend

end program samplebutton
