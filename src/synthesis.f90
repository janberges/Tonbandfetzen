! From Kevin Karplus and Alex Strong, Digital Synthesis of Plucked-String and
! Drum Timbre, Comput. Music J. 7, 43 (1983), https://doi.org/10.2307/3680062.
! Implementation inspired by Vincent Magnin's Fortran synthesizer "ForSynth",
! see https://vmagnin.github.io/forsynth.

module synthesis
   use constants, only: dp
   use lcg, only: minstd
   use samples, only: sample
   implicit none
   private

   public :: karplus_strong

contains

   subroutine karplus_strong(y, period, stretch, blend, tune)
      real(dp), intent(out) :: y(:)
      real(dp), intent(in) :: period, stretch, blend
      logical, intent(in) :: tune

      integer :: p, t
      real(dp) :: r, v, w

      r = max(period, 1.0_dp)

      if (tune) then
         p = nint(r)
         v = 0.5_dp + r - p
      else
         p = floor(r) ! signal frequency is 1 / (p + 1/2)
         v = 1.0_dp
      end if

      w = 1.0_dp - v

      call sample(y(1:min(p + 1, size(y))), 'wave', 'random')

      do t = p + 2, size(y)
         call minstd(r)
         if (r .ge. 1 / stretch) then
            y(t) = y(t - p)
         else
            y(t) = 0.5_dp * (v * y(t - p - 1) + y(t - p) + w * y(t - p + 1))
         end if

         call minstd(r)
         if (r .ge. blend) y(t) = -y(t)
      end do
   end subroutine karplus_strong
end module synthesis
