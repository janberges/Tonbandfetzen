! From Kevin Karplus and Alex Strong, Digital Synthesis of Plucked-String and
! Drum Timbre, Comput. Music J. 7, 43 (1983), https://doi.org/10.2307/3680062.
! Implementation inspired by Vincent Magnin's Fortran synthesizer "ForSynth",
! see https://vmagnin.github.io/forsynth.

module synthesis
   use constants, only: dp
   use lcg, only: minstd
   implicit none
   private

   public :: karplus_strong

contains

   subroutine karplus_strong(y, t, period, blend, decay, tuned)
      real(dp), intent(inout) :: y(:)
      integer, intent(in) :: t
      real(dp), intent(in) :: period, blend, decay
      logical, intent(in) :: tuned

      integer :: p
      real(dp) :: r, v, w

      r = max(period, 1.0_dp)

      if (tuned) then
         p = nint(r)
         v = 0.5_dp + r - p
      else
         p = floor(r) ! signal frequency is 1 / (p + 1/2)
         v = 1.0_dp
      end if

      w = 1.0_dp - v

      call minstd(r)

      if (t .lt. p + 2) then
         y(t) = 2.0_dp * r - 1.0_dp
      else if (r .ge. decay) then
         y(t) = y(t - p)
      else
         y(t) = 0.5_dp * (v * y(t - p - 1) + y(t - p) + w * y(t - p + 1))
      end if

      call minstd(r)
      if (r .ge. blend) y(t) = -y(t)
   end subroutine karplus_strong
end module synthesis
