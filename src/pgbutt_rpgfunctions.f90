module pgbutt_rpgfunctions
    use pgbutt_viewport
    implicit none

contains

    !--------------------------------------------------------------------------
    subroutine rpgbegin(verbose_)
        implicit none
        logical, intent(in), optional :: verbose_

        integer :: ldev
        integer :: idgraphics, pgopen
        character(len=255) :: defdev
        logical :: verbose

        ! default values
        verbose = .false.
        if (present(verbose_)) verbose = verbose_

        ! default graphics device
        call grgenv('DEV', defdev, ldev)
        if (verbose) then
            print *, 'defdev: ', defdev(1:len_trim(defdev))
            print *, 'ldev:', ldev
        end if

        idgraphics = pgopen(defdev)

        call pgvport(0., 1., 0., 1.)
        call pgwindow(0., 1., 0., 1.)
        if (verbose) then
            call pgsci(7)
            call pgmove(0., 0.)
            call pgdraw(1., 0.)
            call pgdraw(1., 1.)
            call pgdraw(0., 1.)
            call pgdraw(0., 0.)
            call pgsci(1)
        end if

        ! Redefine colors
        !   0: black
        !   1: white
        call pgscr(12,0.5,0.5,0.5) ! intermediate gray 
        call pgscr(13,0.7,0.7,0.7) ! light gray
        call pgscr(14,0.3,0.3,0.3) ! dark gray
        call pgscr(15,0.6,0.6,0.6) ! almost intermediate gray

    end subroutine rpgbegin

    !--------------------------------------------------------------------------
    subroutine rpgenv(ptr, xmin, xmax, ymin, ymax, just, axis)
        implicit none
        type(Viewport), pointer, intent(inout) :: ptr
        real, intent(in) :: xmin, xmax, ymin, ymax
        integer, intent(in) :: just, axis

        character(len=10) :: xopts, yopts

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

        ncolor = 0
        if (present(ncolor_)) ncolor = ncolor_
        call pgqci(oldci)
        call pgsci(ncolor)
        call pgrect(ptr%x1v, ptr%x2v, ptr%y1v, ptr%y2v)
        call pgsci(oldci)
        call rpgenv(ptr, ptr%x1w, ptr%x2w, ptr%y1w, ptr%y2w, 0, 0)
        call pgmove(ptr%x1v, ptr%y1v)
        call pgdraw(ptr%x2v, ptr%y1v)
        call pgdraw(ptr%x2v, ptr%y2v)
        call pgdraw(ptr%x1v, ptr%y2v)
        call pgdraw(ptr%x1v, ptr%y1v)
    end subroutine rpgerasw

end module pgbutt_rpgfunctions
