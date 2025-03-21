module pgbutt_canvas
    use pgbutt_viewport
    use pgbutt_linked_viewports
    implicit none

    type :: CanvasDef
        type(LinkedViewports) :: list_button_viewports
        type(LinkedViewports) :: list_plot_viewports
    contains
        procedure :: add_button_viewport
        procedure :: add_plot_viewport
        procedure :: plot_viewport_boundaries
    end type CanvasDef

contains

    !--------------------------------------------------------------------------
    !> Add a generic viewport
    !--------------------------------------------------------------------------
    subroutine add_viewport(list, x1v, x2v, y1v, y2v)
        implicit none
        type(LinkedViewports) :: list
        real :: x1v, x2v, y1v, y2v
        type(Viewport), pointer :: new_viewport, current

        allocate(new_viewport)
        new_viewport%x1v = x1v
        new_viewport%x2v = x2v
        new_viewport%y1v = y1v
        new_viewport%y2v = y2v
        new_viewport%next => null()

        if (.not. associated(list%head)) then
            list%head => new_viewport
        else
          current => list%head
            do while (associated(current%next))
                current => current%next
            end do
            current%next => new_viewport
        end if
        list%count = list%count + 1   ! Increment the counter
    end subroutine add_viewport

    !--------------------------------------------------------------------------
    !> Add a button viewport
    !--------------------------------------------------------------------------
    subroutine add_button_viewport(this, x1v, x2v, y1v, y2v)
        implicit none
        class(CanvasDef), intent(inout) :: this
        real :: x1v, x2v, y1v, y2v
        call add_viewport(this%list_button_viewports, x1v, x2v, y1v, y2v)
    end subroutine add_button_viewport

    !--------------------------------------------------------------------------
    !> Add a plot viewport
    !--------------------------------------------------------------------------
    subroutine add_plot_viewport(this, x1v, x2v, y1v, y2v)
        implicit none
        class(CanvasDef), intent(inout) :: this
        real :: x1v, x2v, y1v, y2v
        call add_viewport(this%list_plot_viewports, x1v, x2v, y1v, y2v)
    end subroutine add_plot_viewport

    !--------------------------------------------------------------------------
    !> Display the boundaries of all the viewports
    !--------------------------------------------------------------------------
    subroutine plot_viewport_boundaries(this, verbose_)
        implicit none
        class(CanvasDef), intent(in) :: this
        logical, intent(in), optional :: verbose_

        integer :: i, num
        integer :: old_line_style
        type(Viewport), pointer :: viewport_ptr
        logical :: verbose

        ! default values
        verbose = .false.
        if (present(verbose_)) verbose = verbose_

        num = this%list_button_viewports%count
        if (num .gt. 0) then
            do i = 1, num
                call this%list_button_viewports%get_viewport(i, viewport_ptr)
                call viewport_ptr%plot_boundary(verbose)
            end do
        end if

        num = this%list_plot_viewports%count
        if (num .gt. 0) then
            do i = 1, num
                call this%list_plot_viewports%get_viewport(i, viewport_ptr)
                call pgqls(old_line_style)
                call pgsls(2)
                call viewport_ptr%plot_boundary(verbose)
                call pgsls(old_line_style)
            end do
        end if
    end subroutine plot_viewport_boundaries

end module pgbutt_canvas
