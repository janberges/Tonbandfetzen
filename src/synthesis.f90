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
      real(dp) :: v, w, r

      if (tune) then
         p = max(nint(period), 1)
         v = 0.5_dp + period - p
         w = 0.5_dp + p - period
      else
         p = max(floor(period), 1) ! signal frequency is 1 / (p + 1/2)
         v = 1.0_dp
         w = 0.0_dp
      end if

      call sample(y(1:min(p + 1, size(y))), 'wave', 'random')

      do t = p + 2, size(y)
         call minstd(r)

         if (r .lt. 1 / stretch) then
            y(t) = 0.5_dp * (v * y(t - p - 1) + y(t - p) + w * y(t - p + 1))
         else
            y(t) = y(t - p)
         end if

         call minstd(r)
         if (r .ge. blend) y(t) = -y(t)
      end do
   end subroutine karplus_strong
end module synthesis
