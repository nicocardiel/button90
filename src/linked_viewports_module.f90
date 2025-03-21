module linked_viewports_module
    use viewport_module
    implicit none
    
    type :: LinkedViewports
        type(Viewport), pointer :: head => null()
        integer :: count = 0  ! Counter to keep track of the number of elements
    contains
        procedure :: get_viewport
        procedure :: print_viewports
    end type LinkedViewports

contains

    subroutine get_viewport(this, position, viewport_ptr)
        implicit none
        class(LinkedViewports), intent(in) :: this
        integer, intent(in) :: position
        type(Viewport), pointer, intent(out) :: viewport_ptr
        integer :: i
        type(Viewport), pointer :: current
        if ((position .lt. 1) .or. (position .gt. this%count)) then
            write(*, '(A,I3)') 'Number of viewport regions defined', this%count
            write(*, '(A,I3,A)') 'Error: position', position, ' out of valid range.'
            stop 'Program terminated due to error.'
        end if
        viewport_ptr => null()
        current => this%head
        do i = 1, position
            if (i == position) then
                viewport_ptr => current
                return
            end if
            current => current%next
        end do
    end subroutine get_viewport

    subroutine print_viewports(this)
        implicit none
        class(LinkedViewports), intent(in) :: this
        type(Viewport), pointer :: current
        current => this%head
        do while (associated(current))
            call current%print_corners()
            current => current%next
        end do
    end subroutine print_viewports

end module linked_viewports_module
