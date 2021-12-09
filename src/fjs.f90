! The Functional Just System (FJS) has been invented by misotanni.
! For a complete description, see https://misotanni.github.io/fjs.
! Here, we only use the microtonal accidentals defined by the FJS.

module fjs
   use constants
   implicit none
   private

   public :: accidentals, comma

contains

   function factorize(n) result(primes)
      integer, allocatable :: primes(:)
      integer, intent(in) :: n

      integer :: i, j

      allocate(primes(1:n))
      primes = 0

      i = n
      j = 2

      do while (i .gt. 1)
         if (modulo(i, j) .eq. 0) then
            primes(j) = primes(j) + 1
            i = i / j
         else
            j = j + 1
         end if
      end do
   end function factorize

   function accidentals(ratio)
      integer, allocatable :: accidentals(:)

      character(*), intent(in) :: ratio

      integer :: i, j, stat
      integer :: numerator, denominator
      integer, allocatable :: primes(:), tmp(:)

      numerator = 1
      denominator = 1

      i = scan(ratio, ':/')
      if (i .eq. 0) i = len(ratio) + 1

      read (ratio(:i - 1), *, iostat=stat) j
      if (stat .eq. 0) numerator = j

      read (ratio(i + 1:), *, iostat=stat) j
      if (stat .eq. 0) denominator = j

      allocate(primes(1:max(numerator, denominator)))
      primes = 0

      tmp = factorize(numerator)
      primes(:size(tmp)) = primes(:size(tmp)) + tmp

      tmp = factorize(denominator)
      primes(:size(tmp)) = primes(:size(tmp)) - tmp

      allocate(accidentals(sum(abs(primes))))

      j = 1
      do i = 1, size(primes)
         accidentals(j:) = sign(i, primes(i))
         j = j + abs(primes(i))
      end do
   end function accidentals

   elemental function red(d)
      real(dp) :: red
      real(dp), intent(in) :: d

      red = d / 2.0_dp ** floor(log(d) / log(2.0_dp))
   end function red

   elemental function reb(d)
      real(dp) :: reb
      real(dp), intent(in) :: d

      reb = red(sqrt(2.0_dp) * red(d)) / sqrt(2.0_dp)
   end function reb

   elemental function cents(d)
      real(dp) :: cents
      real(dp), intent(in) :: d

      cents = 1200.0_dp * log(d) / log(2.0_dp)
   end function cents

   elemental function error(d)
      real(dp) :: error
      real(dp), intent(in) :: d

      error = abs(cents(reb(d)))
   end function error

   elemental function master(d) result(f)
      integer :: f
      real(dp), intent(in) :: d

      real(dp) :: tol

      tol = error(65.0_dp / 63.0_dp)

      f = 0
      do while (error(d / 3.0_dp ** f) .ge. tol)
         f = -f
         if (f .ge. 0) f = f + 1
      end do
   end function master

   elemental function comma(p)
      real(dp) :: comma
      integer, intent(in) :: p

      real(dp), parameter :: Pyth_comma = 3.0_dp ** 12.0_dp / 2.0_dp ** 19.0_dp
      integer :: d

      d = abs(p)

      if (d .eq. 3) then
         comma = Pyth_comma
      else
         comma = reb(d / 3.0_dp ** master(real(d, dp)))
      end if

      if (p .lt. 0) comma = 1.0_dp / comma
   end function comma
end module fjs
