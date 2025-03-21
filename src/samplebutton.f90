! gfortran -o test test.f90 *_module.f90 -L/opt/homebrew/opt/libx11/lib -L/opt/homebrew/opt/pgplot/lib -lpgplot -lX11

program test_canvas
    use pgbutt_canvas
    implicit none

    integer :: ldev
    integer :: idgraphics, pgopen
    character(len=255):: defdev

    type(CanvasDef) :: canvas

    ! default graphics device
    call grgenv('DEV', defdev, ldev)
    print *, 'defdev:', defdev(1:len_trim(defdev))
    print *, 'ldev:', ldev

    idgraphics = pgopen(defdev)

    call pgenv(0., 1., 0., 1., 0, -2)
    ! Redefinimos colores (salida grafica con grises)
    !   0: negro
    !   1: blanco
    !  12: gris intermedio
    !  13: gris claro
    !  14: gris oscuro
    !  15: casi gris intermedio
    call pgscr(12,0.5,0.5,0.5)
    call pgscr(13,0.7,0.7,0.7)
    call pgscr(14,0.3,0.3,0.3)
    call pgscr(15,0.6,0.6,0.6)

    print *, canvas%list_button_viewports%count
    print *, canvas%list_plot_viewports%count
    call canvas%add_button_viewport(0.05, 0.95, 0.80, 0.95)
    call canvas%add_button_viewport(0.05, 0.95, 0.60, 0.75)
    call canvas%add_plot_viewport(0.05, 0.95, 0.10, 0.25)
    print *, canvas%list_button_viewports%count
    print *, canvas%list_plot_viewports%count

    call canvas%plot_viewport_boundaries(.FALSE.)

    write(*,'(a)',advance="no") 'Press <RETURN> to continue...'
    read(*,*)
    call canvas%list_button_viewports%print_viewports()

    !end of program
    call pgend

end program test_canvas
