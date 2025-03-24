module pgbutt_rpgfunctions
    use pgbutt_viewport
    implicit none

contains

    !--------------------------------------------------------------------------
    subroutine rpgenv(ptr, xmin, xmax, ymin, ymax, just, axis)
        implicit none
        type(Viewport), pointer, intent(inout) :: ptr
        real, intent(in) :: xmin, xmax, ymin, ymax
        integer, intent(in) :: just, axis

        character(len=10) :: xopts, yopts

        ! protection
        if (ptr%idn.eq.0) then
            stop 'ERROR: graphic device has not been opened'
        else
            call pgslct(ptr%idn)
        end if

        if(xmin.eq.xmax)then
            write(*, '(a)')'Invalid x limits in rpgenv: xmin = xmax.'
            return
        else if(ymin.eq.ymax)then
            write(*, '(a)')'Invalid y limits in rpgenv: ymin = ymax.'
            return
        end if

        call pgvport(ptr%x1v, ptr%x2v, ptr%y1v, ptr%y2v)

        if(just.eq.1)then
            call pgwnad(xmin,xmax,ymin,ymax)
        else
            call pgswin(xmin,xmax,ymin,ymax)
        end if
        ptr%x1w = xmin
        ptr%x2w = xmax
        ptr%y1w = ymin
        ptr%y2w = ymax
        !
        yopts='*'
        if(axis.eq.-2)then
            xopts= char(32)
        else if(axis.eq.-1)then
            xopts='bc'
        else if(axis.eq.0)then
            xopts='bcnst'
        else if(axis.eq.1)then
            xopts='abcnst'
        else if(axis.eq.2)then
            xopts='abcgnst'
        else if(axis.eq.10)then
            xopts='bcnstl'
            yopts='bcnst'
        else if(axis.eq.20)then
            xopts='bcnst'
            yopts='bcnstl'
        else if(axis.eq.30)then
            xopts='bcnstl'
            yopts='bcnstl'
        else
            write(*,'(a)')'rpgenv: illegal axis argument.'
            xopts='bcnst'
        end if
        if(yopts.eq.'*') yopts=xopts
        ! draw box
        call pgbox(xopts,0.0,0,yopts,0.0,0)

        call pgvport(0.,1.,0.,1.)
        call pgwindow(0.,1.,0.,1.)

    end subroutine rpgenv

    !--------------------------------------------------------------------------
    subroutine rpgline(ptr, n, x, y)
        implicit none
        type(Viewport), pointer, intent(in) :: ptr
        integer, intent(in) :: n
        real, intent(in) :: x(:), y(:)

        ! protection
        if (ptr%idn.eq.0) then
            stop 'ERROR: graphic device has not been opened'
        else
            call pgslct(ptr%idn)
        end if

        call pgvport(ptr%x1v, ptr%x2v, ptr%y1v, ptr%y2v)
        call pgwindow(ptr%x1w, ptr%x2w, ptr%y1w, ptr%y2w)
        call pgline(n, x, y)
        call pgvport(0.,1.,0.,1.)
        call pgwindow(0.,1.,0.,1.)
    end subroutine rpgline

    !--------------------------------------------------------------------------
    subroutine rpgerasw(ptr, ncolor_)
        implicit none
        type(Viewport), pointer, intent(inout) :: ptr
        integer, intent(in), optional :: ncolor_

        integer :: ncolor, oldci

        ! protection
        if (ptr%idn.eq.0) then
            stop 'ERROR: graphic device has not been opened'
        else
            call pgslct(ptr%idn)
        end if

        ncolor = 0
        if (present(ncolor_)) ncolor = ncolor_
        call pgqci(oldci)
        call pgsci(ncolor)
        call pgrect(ptr%x1v, ptr%x2v, ptr%y1v, ptr%y2v)
        call pgsci(oldci)
        call rpgenv(ptr, ptr%x1w, ptr%x2w, ptr%y1w, ptr%y2w, ptr%just, ptr%axis)
        call pgmove(ptr%x1v, ptr%y1v)
        call pgdraw(ptr%x2v, ptr%y1v)
        call pgdraw(ptr%x2v, ptr%y2v)
        call pgdraw(ptr%x1v, ptr%y2v)
        call pgdraw(ptr%x1v, ptr%y1v)
    end subroutine rpgerasw

end module pgbutt_rpgfunctions
