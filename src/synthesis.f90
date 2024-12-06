! From Kevin Karplus and Alex Strong, Digital Synthesis of Plucked-String and
! Drum Timbre, Comput. Music J. 7, 43 (1983), https://doi.org/10.2307/3680062.
! Implementation inspired by Vincent Magnin's Fortran synthesizer "ForSynth",
! see https://vmagnin.github.io/forsynth.

module synthesis
   use constants, only: dp
   use samples, only: sample
   implicit none
   private

   public :: plucked_string

contains

   subroutine plucked_string(y, period)
      real(dp), intent(out) :: y(:)
      real(dp), intent(in) :: period

      integer :: p, t

      p = floor(period) ! resulting pitch corresponds to period p + 1/2

      call sample(y(1:min(p + 1, size(y))), 'wave', 'random')

      do t = p + 2, size(y)
         y(t) = 0.5_dp * (y(t - p) + y(t - p - 1))
      end do
   end subroutine plucked_string
end module synthesis
