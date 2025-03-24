module pgbutt_canvas
    use pgbutt_viewport
    use pgbutt_linked_viewports
    use pgbutt_button
    implicit none

    type :: CanvasDef
        integer :: idn=0
        type(LinkedViewports) :: list_button_viewports
        type(LinkedViewports) :: list_plot_viewports
    contains
        procedure :: get_viewport_from_mouse
        procedure :: get_button_from_mouse
        procedure :: add_button_viewport
        procedure :: add_plot_viewport
        procedure :: plot_viewport_boundaries
        procedure :: rpgopen
    end type CanvasDef

contains

    !--------------------------------------------------------------------------
    !> Return viewport corresponding to current mouse location
    !--------------------------------------------------------------------------
    subroutine get_viewport_from_mouse(this, xc, yc, viewport_ptr, verbose_)
        implicit none
        class(CanvasDef), intent(inout) :: this
        real, intent(in) :: xc, yc
        type(Viewport), pointer, intent(out) :: viewport_ptr
        logical, intent(in), optional :: verbose_

        integer :: i, num
        logical :: verbose

        ! protection
        if (this%idn.eq.0) then
            stop 'ERROR: graphic device has not been opened'
        else
            call pgslct(this%idn)
        end if

        ! default values
        verbose = .false.
        if (present(verbose_)) verbose = verbose_

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
    !> Return viewport and button corresponding to current mouse location
    !--------------------------------------------------------------------------
    subroutine get_button_from_mouse(this, xc, yc, ch, viewport_ptr, nb, verbose_)
        implicit none
        class(CanvasDef), intent(inout) :: this
        real, intent(out) :: xc, yc
        character(len=1), intent(out) :: ch
        type(Viewport), pointer, intent(out) :: viewport_ptr
        integer, intent(out) :: nb
        logical, intent(in), optional :: verbose_

        logical :: verbose

        ! protection
        if (this%idn.eq.0) then
            stop 'ERROR: graphic device has not been opened'
        else
            call pgslct(this%idn)
        end if

        ! default values
        verbose = .false.
        if (present(verbose_)) verbose = verbose_

        call pgcurs(xc, yc, ch)
        nb = 0
        call this%get_viewport_from_mouse(xc, yc, viewport_ptr, verbose)
        if (associated(viewport_ptr)) then
            call viewport_ptr%ifbutton(xc, yc, nb)
            if (verbose) then
                print *, 'Found #', viewport_ptr%number
                print *, 'nb=', nb
            end if
        end if

    end subroutine get_button_from_mouse

    !--------------------------------------------------------------------------
    !> Add a generic viewport
    !--------------------------------------------------------------------------
    subroutine add_viewport(list, idn, x1v, x2v, y1v, y2v, &
                            nx_, ny_, x1w_, x2w_, y1w_, y2w_, &
                            new_viewport)
        implicit none
        type(LinkedViewports), intent(inout) :: list
        integer, intent(in) :: idn
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
        new_viewport%idn = idn
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

        ! protection
        if (this%idn.eq.0) then
            stop 'ERROR: graphic device has not been opened'
        else
            call pgslct(this%idn)
        end if

        if (nx .lt. 1) then
            print *, 'Invalid nx=', nx
        end if
        if (ny .lt. 1) then
            print *, 'Invalid ny=', ny
        end if
        call add_viewport(this%list_button_viewports, &
                          this%idn, &
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

        ! protection
        if (this%idn.eq.0) then
            stop 'ERROR: graphic device has not been opened'
        else
            call pgslct(this%idn)
        end if

        call add_viewport(this%list_plot_viewports, &
                          this%idn, &
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

        ! protection
        if (this%idn.eq.0) then
            stop 'ERROR: graphic device has not been opened'
        else
            call pgslct(this%idn)
        end if

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

    !--------------------------------------------------------------------------
    !> Open graphics device
    !--------------------------------------------------------------------------
    subroutine rpgopen(this, device, verbose_)
        implicit none
        class(CanvasDef), intent(inout) :: this
        character(len=*), intent(in) :: device
        logical, intent(in), optional :: verbose_

        integer :: pgopen, istatus
        logical :: verbose

        ! default values
        verbose = .false.
        if (present(verbose_)) verbose = verbose_

        do
            istatus = pgopen(device)
            if (istatus.gt.0) then
                this%idn = istatus
                exit
            else
                print *, 'istatus:', istatus
                stop 'ERROR calling pgopen'
            end if
        end do
        call pgask(.false.)

        call pgvport(0., 1., 0., 1.)
        call pgwindow(0., 1., 0., 1.)
        if (verbose) then
            call pgsci(7)
            call pgmove(0.01, 0.01)
            call pgdraw(0.99, 0.01)
            call pgdraw(0.99, 0.99)
            call pgdraw(0.01, 0.99)
            call pgdraw(0.01, 0.01)
            call pgsci(1)
        end if

        ! Redefine colors
        !   0: black
        !   1: white
        call pgscr(12,0.5,0.5,0.5) ! intermediate gray 
        call pgscr(13,0.7,0.7,0.7) ! light gray
        call pgscr(14,0.3,0.3,0.3) ! dark gray
        call pgscr(15,0.6,0.6,0.6) ! almost intermediate gray

    end subroutine rpgopen

end module pgbutt_canvas
