program samplebutton
    use pgbutt_rpgfunctions
    use pgbutt_canvas
    implicit none

    type(CanvasDef) :: canvas

    !open graphics output
    call rpgbegin()

    print *, canvas%list_button_viewports%count
    print *, canvas%list_plot_viewports%count
    call canvas%add_button_viewport(0.0, 1.0, 0.80, 0.95)
    call canvas%add_button_viewport(0.0, 1.0, 0.60, 0.75)
    call canvas%add_plot_viewport(0.0, 1.0, 0.10, 0.25)
    print *, canvas%list_button_viewports%count
    print *, canvas%list_plot_viewports%count

    call canvas%plot_viewport_boundaries()

    write(*,'(a)',advance="no") 'Press <RETURN> to continue...'
    read(*,*)
    call canvas%list_button_viewports%print_viewports()

    !end of program
    call pgend

end program samplebutton
