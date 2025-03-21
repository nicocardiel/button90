module pgbutt_canvas
    use pgbutt_viewport
    use pgbutt_linked_viewports
    use pgbutt_button
    implicit none

    type :: CanvasDef
        type(LinkedViewports) :: list_button_viewports
        type(LinkedViewports) :: list_plot_viewports
    contains
        procedure :: get_viewport_from_mouse
        procedure :: add_button_viewport
        procedure :: add_plot_viewport
        procedure :: plot_viewport_boundaries
    end type CanvasDef

contains

    !--------------------------------------------------------------------------
    !> Return the viewport corresponding to the current mouse location
    !--------------------------------------------------------------------------
    subroutine get_viewport_from_mouse(this, xc, yc, verbose, viewport_ptr)
        implicit none
        class(CanvasDef), intent(inout) :: this
        real, intent(in) :: xc, yc
        logical :: verbose
        type(Viewport), pointer, intent(out) :: viewport_ptr

        integer :: i, num

        ! find the first viewport enclosing (XC, YC)
        num = this%list_button_viewports%count
        if (num .gt. 0) then
            do i = 1, num
                call this%list_button_viewports%get_viewport(i, viewport_ptr)
                if (viewport_ptr%active) then
                    if (viewport_ptr%mouse_inside(xc, yc)) then
                        if (verbose) then
                            print *, xc, yc, ' -> Button viewport #', i
                        end if
                        return
                    end if
                end if
            end do
        end if

        num = this%list_plot_viewports%count
        if (num .gt. 0) then
            do i = 1, num
                call this%list_plot_viewports%get_viewport(i, viewport_ptr)
                if (viewport_ptr%active) then
                    if (viewport_ptr%mouse_inside(xc, yc)) then
                        if (verbose) then
                            print *, xc, yc, ' -> Plot viewport #', i
                        end if
                        return
                    end if
                end if
            end do
        end if

        ! mouse outside any viewport
        viewport_ptr => null()
    end subroutine get_viewport_from_mouse

    !--------------------------------------------------------------------------
    !> Add a generic viewport
    !--------------------------------------------------------------------------
    subroutine add_viewport(list, x1v, x2v, y1v, y2v, &
                            nx_, ny_, x1w_, x2w_, y1w_, y2w_, &
                            new_viewport)
        implicit none
        type(LinkedViewports), intent(inout) :: list
        real, intent(in) :: x1v, x2v, y1v, y2v
        integer, intent(in), optional :: nx_, ny_
        real, intent(in), optional :: x1w_, x2w_, y1w_, y2w_
        type(Viewport), pointer, intent(out) :: new_viewport

        integer :: i, j, k
        type(Viewport), pointer :: current
        integer :: nx, ny
        real :: x1w, x2w, y1w, y2w
        real :: dx, dy
        real :: x1, x2, y1, y2
        real :: ddx, ddy
        real :: xgap, ygap
        type(SingleButton), allocatable :: button_array(:)

        ! default values
        nx = 0
        if (present(nx_)) nx = nx_
        ny = 0
        if (present(ny_)) ny = ny_
        x1w = 0.0
        if (present(x1w_)) x1w = x1w_
        x2w = 1.0
        if (present(x2w_)) x2w = x2w_
        y1w = 0.0
        if (present(y1w_)) y1w = y1w_
        y2w = 1.0
        if (present(y2w_)) y2w = y2w_

        allocate(new_viewport)
        new_viewport%x1v = x1v
        new_viewport%x2v = x2v
        new_viewport%y1v = y1v
        new_viewport%y2v = y2v
        new_viewport%nx = nx
        new_viewport%ny = ny
        allocate(button_array(nx*ny))
        k = 0
        dx = (x2v - x1v) / nx
        dy = (y2v - y1v) / ny
        do j = ny, 1, -1
            y1 = y1v + (j - 1) * dy 
            y2 = y1v + j * dy 
            ddy = (y2 - y1) / 7.
            ygap = ddy / 10
            do i = 1, nx
                x1 = x1v + (i - 1) * dx 
                x2 = x1v + i * dx 
                ddx = (x2 - x1) / 20.
                xgap = ddx / 10
                k = k + 1
                button_array(k)%num = k
                button_array(k)%x1 = x1
                button_array(k)%x2 = x2
                button_array(k)%y1 = y1
                button_array(k)%y2 = y2
                button_array(k)%ddx = ddx
                button_array(k)%ddy = ddy
                button_array(k)%xgap = xgap
                button_array(k)%ygap = ygap
                button_array(k)%offy = (y2 - y1) * button_array(k)%ytext
                button_array(k)%text = 'None'
            end do
        end do
        new_viewport%button_array = button_array
        new_viewport%x1w = x1w
        new_viewport%x2w = x2w
        new_viewport%y1w = y1w
        new_viewport%y2w = y2w
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
        new_viewport%number = list%count
        new_viewport%active = .true.
    end subroutine add_viewport

    !--------------------------------------------------------------------------
    !> Add a button viewport
    !--------------------------------------------------------------------------
    subroutine add_button_viewport(this, x1v, x2v, y1v, y2v, nx, ny, &
                                   new_viewport)
        implicit none
        class(CanvasDef), intent(inout) :: this
        real, intent(in) :: x1v, x2v, y1v, y2v
        integer, intent(in) :: nx, ny
        type(Viewport), pointer, intent(out) :: new_viewport

        if (nx .lt. 1) then
            print *, 'Invalid nx=', nx
        end if
        if (ny .lt. 1) then
            print *, 'Invalid ny=', ny
        end if
        call add_viewport(this%list_button_viewports, &
                          x1v, x2v, y1v, y2v, &
                          nx_=nx, ny_=ny, &
                          new_viewport=new_viewport)
    end subroutine add_button_viewport

    !--------------------------------------------------------------------------
    !> Add a plot viewport
    !--------------------------------------------------------------------------
    subroutine add_plot_viewport(this, x1v, x2v, y1v, y2v, &
                               new_viewport)
        implicit none
        class(CanvasDef), intent(inout) :: this
        real :: x1v, x2v, y1v, y2v
        type(Viewport), pointer, intent(out) :: new_viewport

        call add_viewport(this%list_plot_viewports, &
                          x1v, x2v, y1v, y2v, &
                          new_viewport=new_viewport)
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
