module pgbutt_viewport
    use pgbutt_button
    implicit none

    type :: Viewport
      ! viewport coordinates
      real :: x1v=0.0, x2v=1.0, y1v=0.0, y2v=1.0
      ! number of buttons in X and Y
      integer :: nx, ny
      ! world coordinates for plot
      real :: x1w=0.0, x2w=1.0, y1w=0.0, y2w=0.0
      ! just and axis por pgenv
      integer :: just=0, axis=0
      ! viewport number in viewport list
      integer :: number
      ! viewport availability
      logical :: active=.true.
      ! buttons
      type(SingleButton), allocatable :: button_array(:)
      ! auxiliary pointer to  next Viewport object
      type(Viewport), pointer :: next => null()
    contains
        procedure :: mouse_inside
        procedure :: plot_boundary
        procedure :: print_corners
        procedure :: button
        procedure :: ifbutton
        procedure :: world
    end type Viewport

contains

    function mouse_inside(this, xc, yc) result(inside)
        implicit none
        class(Viewport), intent(in) :: this
        real, intent(in) :: xc, yc
        logical :: inside
        if ( (xc .ge. this%x1v) .and. &
             (xc .le. this%x2v) .and. &
             (yc .ge. this%y1v) .and. &
             (yc .le. this%y2v) ) then
            inside = .true.
        else
            inside = .false.
        end if
    end function mouse_inside
    
    !--------------------------------------------------------------------------
    subroutine plot_boundary(this, verbose_)
        implicit none
        class(Viewport), intent(in) :: this
        logical, intent(in), optional :: verbose_

        real :: x(5), y(5)
        logical :: verbose

        ! default values
        verbose = .false.
        if (present(verbose_)) verbose = verbose_

        if(verbose)then
            print *, 'xv1:', this%x1v
            print *, 'xv2:', this%x2v
            print *, 'yv1:', this%y1v
            print *, 'yv2:', this%y2v
        end if
        x(1) = this%x1v
        y(1) = this%y1v
        x(2) = this%x2v
        y(2) = this%y1v
        x(3) = this%x2v
        y(3) = this%y2v
        x(4) = this%x1v
        y(4) = this%y2v
        x(5) = this%x1v
        y(5) = this%y1v
        call pgline(5, x, y)
    end subroutine plot_boundary

    !--------------------------------------------------------------------------
    subroutine print_corners(this)
        implicit none
        class(Viewport), intent(in) :: this
        print *, '-------'
        print *, this%x1v, this%x2v, this%y1v, this%y2v
        print *, this%nx, this%ny
        print *, this%x1w, this%x2w, this%y1w, this%y2w
    end subroutine print_corners

    !--------------------------------------------------------------------------
    subroutine button(this, num, text, mode)
        implicit none
        class(Viewport), intent(inout) :: this
        integer, intent(in) :: num
        character(len=*), intent(in) :: text
        integer, intent(in) :: mode

        integer :: num_max
        integer :: lmode, ncolortext
        integer :: pgscf_old
        real :: x1, x2, y1, y2
        real :: ddx, ddy
        real :: xgap, ygap
        real :: offy
        real :: x1w, x2w, y1w, y2w
        real :: x1v, x2v, y1v, y2v
        real :: pgsch_old
        real :: xp(4), yp(4)

        num_max = size(this%button_array)

        if (num < 1) then
            print *, 'num:', num
            stop 'Button number < 1'
        end if

        if (num > num_max) then
            print *, 'num:', num
            print *, 'num_max:', num_max
            stop 'Button number > num_max'
        end if

        lmode = mode
        ncolortext = 0

        if (mode.gt.5) then
            print *, 'mode:', mode
            stop 'ERROR: invalid button mode'
        end if

        if (mode.lt.-1) then
            ncolortext = -mode - 1
            lmode = 2
        end if

        if (lmode.eq.-1) then
            this%button_array(num)%exist = .false.
        elseif (lmode.eq.3) then
            this%button_array(num)%exist = .false.
        else
            this%button_array(num)%exist = .true.
        end if

        x1 = this%button_array(num)%x1
        x2 = this%button_array(num)%x2
        y1 = this%button_array(num)%y1
        y2 = this%button_array(num)%y2
        ddx = this%button_array(num)%ddx
        ddy = this%button_array(num)%ddy
        offy = this%button_array(num)%offy

        xgap = this%button_array(num)%xgap
        ygap = this%button_array(num)%ygap

        x1 = x1 + xgap
        x2 = x2 - xgap
        y1 = y1 + ygap
        y2 = y2 - ygap

        call pgqwin(x1w, x2w, y1w, y2w)
        call pgqvp(0, x1v, x2v, y1v, y2v)
        call pgvport(this%x1v, this%x2v, this%y1v, this%y2v)
        call pgwindow(this%x1v, this%x2v, this%y1v, this%y2v)

        if (lmode.eq.-1) then
            call pgsci(0)
            call pgrect(x1-xgap, x2+xgap, y1-ygap, y2+ygap)
            call pgsci(1)
        else
            call pgqcf(pgscf_old)
            call pgscf(this%button_array(num)%pgscf)
            call pgqch(pgsch_old)
            call pgsch(this%button_array(num)%pgsch)

            if((lmode.ge.1).and.(lmode.le.3))then
                if(lmode.eq.1)then
                    call pgsci(1)
                elseif(lmode.eq.2)then
                    call pgsci(ncolortext)
                elseif(lmode.eq.3)then
                    this%button_array(num)%exist=.false.
                    call pgsci(15)
                end if
                call pgptext((x1+x2)/2., y1+offy, 0., 0.5, trim(text))
            else
                call pgsci(12)
                call pgrect(x1+ddx, x2-ddx, y1+ddy, y2-ddy)
                if(lmode.eq.0)then
                    call pgsci(13)
                else
                    call pgsci(14)
                end if
                call subdata(xp, x1, x1+ddx, x1+ddx, x1)
                call subdata(yp, y1, y1+ddy, y2-ddy, y2)
                call pgpoly(4, xp, yp)
                call subdata(xp, x1, x1+ddx, x2-ddx, x2)
                call subdata(yp, y2, y2-ddy, y2-ddy, y2)
                call pgpoly(4, xp, yp)
                if(lmode.eq.0)then
                    call pgsci(14)
                else
                    call pgsci(13)
                end if
                call subdata(xp, x1, x1+ddx, x2-ddx, x2)
                call subdata(yp, y1, y1+ddy, y1+ddy, y1)
                call pgpoly(4, xp, yp)
                call subdata(xp, x2, x2-ddx, x2-ddx, x2)
                call subdata(yp, y1, y1+ddy, y2-ddy, y2)
                call pgpoly(4, xp, yp)
                if(lmode.eq.0)then
                    call pgsci(1)
                else
                    call pgsci(0)
                end if
                call pgmove(x1, y2)
                call pgdraw(x1+ddx, y2-ddy)
                if(lmode.eq.0)then
                    call pgsci(0)
                else
                    call pgsci(1)
                end if
                call pgmove(x2, y1)
                call pgdraw(x2-ddx, y1+ddy)
                if(lmode.eq.5)then
                    call pgsci(1)
                else
                    call pgsci(0)
                end if
                call pgptext((x1+x2)/2., y1+offy, 0., 0.5, trim(text))
                call pgsci(0)
                call pgmove(x1, y1)
                call pgdraw(x2, y1)
                call pgdraw(x2, y2)
                call pgdraw(x1, y2)
                call pgdraw(x1, y1)
                call pgsci(1)
            end if

            call pgscf(pgscf_old)
            call pgsch(pgsch_old)
            if (lmode.eq.3) call pgsci(1)
            if (lmode.eq.2) call pgsci(1)
        end if

        call pgvport(x1v, x2v, y1v, y2v)
        call pgwindow(x1w, x2w, y1w, y2w)
        

    end subroutine button

    !--------------------------------------------------------------------------
    !> Auxiliary function for subroutine button
    subroutine subdata(xx, x1, x2, x3, x4)
        implicit none
        real, intent(out) :: xx(4)
        real, intent(in) :: x1, x2, x3, x4
        xx(1) = x1
        xx(2) = x2
        xx(3) = x3
        xx(4) = x4
    end subroutine subdata

    !--------------------------------------------------------------------------
    subroutine ifbutton(this, xc, yc, nb)
        implicit none
        class(Viewport), intent(in) :: this
        real, intent(in) :: xc, yc
        integer, intent(out) :: nb

        integer :: i

        nb = 0
        if (this%nx * this%ny .eq.0) then
            return
        end if

        if (mouse_inside(this, xc, yc)) then
            do i = 1, this%nx * this%ny
                if (this%button_array(i)%exist) then
                    if ( (this%button_array(i)%x1 .le. xc) .and. &
                         (this%button_array(i)%x2 .ge. xc) .and. &
                         (this%button_array(i)%y1 .le. yc) .and. &
                         (this%button_array(i)%y2 .ge. yc) ) then
                        nb = i
                        return
                    end if
                end if
            end do
        end if
    end subroutine ifbutton

    !--------------------------------------------------------------------------
    subroutine world(this, xc, yc, xworld, yworld)
        implicit none
        class(Viewport), intent(in) :: this
        real, intent(in) :: xc, yc
        real, intent(out) :: xworld, yworld

        real :: x1, x2, y1, y2

        call pgvport(this%x1v, this%x2v, this%y1v, this%y2v)
        call pgwindow(this%x1w, this%x2w, this%y1w, this%y2w)
        call pgqvp(0, x1, x2, y1, y2)
        xworld = this%x1w + (xc - x1)/(x2 - x1) * (this%x2w - this%x1w)
        yworld = this%y1w + (yc - y1)/(y2 - y1) * (this%y2w - this%y1w)
        call pgvport(0.,1.,0.,1.)
        call pgwindow(0.,1.,0.,1.)
    end subroutine world

end module pgbutt_viewport
