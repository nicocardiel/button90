module pgbutt_rpgfunctions
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

        call pgenv(0., 1., 0., 1., 0, -2)
        ! Redefine colors
        !   0: black
        !   1: white
        call pgscr(12,0.5,0.5,0.5) ! intermediate gray 
        call pgscr(13,0.7,0.7,0.7) ! light gray
        call pgscr(14,0.3,0.3,0.3) ! dark gray
        call pgscr(15,0.6,0.6,0.6) ! almost intermediate gray

    end subroutine rpgbegin

end module pgbutt_rpgfunctions
