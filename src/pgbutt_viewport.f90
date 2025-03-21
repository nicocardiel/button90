module pgbutt_viewport
    implicit none

    type :: Viewport
      real :: x1v, x2v, y1v, y2v
      type(Viewport), pointer :: next => null()
    contains
        procedure :: plot_boundary
        procedure :: print_corners
    end type Viewport

contains

    subroutine plot_boundary(this, verbose)
        implicit none
        class(Viewport), intent(in) :: this
        logical, intent(in) :: verbose
        real :: x(5), y(5)
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

    subroutine print_corners(this)
        implicit none
        class(Viewport), intent(in) :: this
        print *, this%x1v, this%x2v, this%y1v, this%y2v
    end subroutine print_corners

end module pgbutt_viewport
