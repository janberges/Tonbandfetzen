module interpreter
   use constants
   use io
   use rationals
   use riff
   use samples
   use search
   implicit none
   private

   public :: play

contains

   subroutine play(notes, tones)
      character(*), intent(in) :: notes
      type(audio), intent(out) :: tones

      character(*), parameter :: initial = '~"`ABCDEFGNSZWPR=-+&?!%[]'

      character(:), allocatable :: symbol, word ! special/lexical string

      real(dp), allocatable ::    & ! sound stages:
         wave(:),                 & !   I. samples
         rise(:), rho(:),         & !  II. segments (polar coordinates)
         fall(:), phi(:), mel(:, :) ! III. melody (channel-wise)

      real(dp), allocatable :: work(:, :)

      logical :: todo(3) ! samples yet to be initialized?

      real(dp) :: x ! exact time
      real(dp) :: b ! beat duration

      integer :: t ! rounded time
      integer :: c ! continuance
      integer :: d ! note duration

      integer :: tmin, tmax, cmax ! extreme times

      real(dp) :: A4 ! concert pitch

      integer :: steps ! notes per octave

      real(dp) :: f  ! frequency f(t)
      real(dp) :: f0 ! reference frequency
      real(dp) :: fi ! initial frequency
      real(dp) :: fd ! f(t + d) / f(t)
      real(dp) :: fb ! f(t + b) / f(t)
      real(dp) :: f1 ! f(t + 1) / f(t)

      complex(dp) :: v  ! vibrato amplitude
      complex(dp) :: v1 ! vibrato frequency

      real(dp) :: a  ! amplitude a(t) = sqrt(L^2 + R^2)
      real(dp) :: a0 ! reference amplitude
      real(dp) :: ai ! initial amplitude
      real(dp) :: ad ! a(t + d) / a(t)
      real(dp) :: ab ! a(t + b) / a(t)
      real(dp) :: a1 ! a(t + 1) / a(t)

      real(dp) :: r  ! amplitudes ratio r(t) = R:L
      real(dp) :: r0 ! reference ratio
      real(dp) :: ri ! initial ratio
      real(dp) :: rd ! r(t + d) / r(t)
      real(dp) :: rb ! r(t + b) / r(t)
      real(dp) :: r1 ! r(t + 1) / r(t)

      real(dp) :: phase ! turn = 1

      integer :: i, j ! arbitrary integers/indices

      real(dp) :: s ! equivalent of a second

      ! time marks
      real(dp) :: marks(0:99)
      real(dp) :: x1, x2, dx
      integer  :: t1, t2, dt, copies

      ! text marks
      integer :: mark, count, info

      real(dp) :: random, factor

      ! tuning
      character(:), allocatable :: tuning

      real(dp), parameter :: equal_fifth = 2.0_dp ** (7.0_dp / 12.0_dp)
      real(dp), parameter :: just_fifth = 1.5_dp
      real(dp), parameter :: syntonic_comma = 1.0125_dp

      integer :: keynote

      tuning = 'equal'
      keynote = -3 ! C

      tones%rate = 44100.0_dp
      tones%channels = 2

      todo = .true.

      call focus(notes)

      do
         select case (next(special, length=1))
            case ('$'); tones%rate = n()
            case ('O'); tones%channels = int(n(), i2)

            case ('~'); todo( 1 ) = .false.
            case ('S'); todo( 2 ) = .false.
            case ('Z'); todo( 3 ) = .false.
            case ('N'); todo(2:3) = .false.

            case ("'", 'none')
               exit
         end select
      end do

      s = tones%rate

      if (todo(1)) call load(wave, 'wave', 'circular', nint(1.0_dp * s))
      if (todo(2)) call load(rise, 'fade', 'circular', nint(0.1_dp * s))
      if (todo(3)) call load(fall, 'fade', 'circular', nint(0.1_dp * s))

      b = 0.5_dp * s

      x = 0.0_dp

      t = 0
      c = 0

      tmin = t
      tmax = t
      cmax = c

      call reset
      marks = 0

      do
         symbol = next(special, length=1)

         tmin = min(tmin, t)
         tmax = max(tmax, t)

         if (done()) then
            cmax = max(cmax, c)
            c = 0
         end if

         select case (symbol)
            case ('|')
               b = n() * s

            case ("'")
               x = x + rational(next(numeral, '1')) * b
               d = nint(x) - t
               c = c + d
               t = t + d

            case ('"', '`')
               x = x + sgn('`"') * rational(next(numeral, '1')) * b
               t = nint(x)

            case ('M')
               marks(int(n())) = x

            case ('W')
               x = marks(int(n()))
               t = nint(x)

            case ('P', 'R')
               x1 = marks(int(n()))
               x2 = marks(int(n()))
               dx = x2 - x1

               if (symbol .eq. 'R') then
                  copies = int(n())
               else
                  copies = 1
               end if

               do i = 1, copies
                  x = x + dx
                  t = nint(x)
               end do

            case ('I')
               call remember(int(n()))

            case ('J')
               mark = int(n())
               count = int(n())

               call get(info)

               if (info .lt. count) then
                  call set(info + 1)
                  call revert(mark)
               else
                  call set(0)
               end if

            case ('*')
               symbol = next('*', length=1)

            case ('none')
               exit
         end select
      end do

      allocate(phi(cmax))
      allocate(rho(cmax))

      tones%points = tmax - tmin

      allocate(mel(tones%channels, tones%points))

      mel(:, :) = 0.0_dp

      b = 0.5_dp * s
      A4 = 440.0_dp / s
      steps = 12

      f0 = A4;     f = f0; fi = f0; fd = 1.0_dp; fb = 1.0_dp
      a0 = 1.0_dp; a = a0; ai = a0; ad = 1.0_dp; ab = 1.0_dp
      r0 = 1.0_dp; r = r0; ri = r0; rd = 1.0_dp; rb = 1.0_dp

      v  = 0.0_dp;
      v1 = 1.0_dp;

      phase = 0.0_dp

      x = 0.0_dp

      t = -tmin
      c = 0

      call reset
      marks = 0

      do
         symbol = next(special, length=1)

         if (done()) then
            f = fi
            a = ai
            r = ri

            i = min(size(rise), c)

            rho(:i) = rho(:i) * rise(:i - 1)

            i = max(c - size(fall) + 1, 1)

            rho(c:i:-1) = rho(c:i:-1) * fall(:c - i)

            i = t - c + 1

            select case (tones%channels)
               case (1)
                  mel(1, i:t) = mel(1, i:t) + rho(:c)

               case (2)
                  mel(1, i:t) = mel(1, i:t) + rho(:c) * cos(phi(:c))
                  mel(2, i:t) = mel(2, i:t) + rho(:c) * sin(phi(:c))
            end select

            c = 0
         end if

         select case (symbol)
            case ('~', 'S', 'Z', 'N')
               word = next(lexical)

               if (word .eq. '#') then
                  i = int(n())
               else
                  i = nint(n() * s)
               end if

               select case (symbol)
                  case ('~'); call load(wave, 'wave', word, i)
                  case ('S'); call load(rise, 'fade', word, i)
                  case ('Z'); call load(fall, 'fade', word, i)
                  case ('N'); call load(rise, 'fade', word, i)
                     fall = rise
               end select

            case ('|')
               b = n() * s

            case ('@')
               A4 = n() / s

            case ('X')
               call random_number(random)
               random = 1.0_dp - 2.0_dp * random
               random = 2.0_dp ** (random * n() / steps)
               A4 = A4 * random
               f0 = f0 * random
               fi = fi * random
               f  = f  * random

            case ('T')
               tuning = next(lexical)

            case ('H')
               steps = nint(n())

            case ('C', 'D', 'E', 'F', 'G', 'A', 'B')
               i = index('FCGDAEB', symbol) - 5

               word = next(lexical, '')

               do j = 1, len(word)
                  select case(word(j:j))
                     case ('b')
                        i = i - 7
                     case ('#')
                        i = i + 7
                  end select
               end do

               select case(tuning)
                  case ('equal')
                     f0 = A4 * equal_fifth ** i

                  case ('fifth', 'just')
                     f0 = A4 * just_fifth ** i
               end select

               ! fold back to first octave:
               j = 4 * i + 5
               j = (j - modulo(j, 7)) / 7
               f0 = f0 / 2.0_dp ** j

               if (tuning .eq. 'just') then
                  j = i + modulo(1 - keynote, 4)
                  j = (j - modulo(j, 4)) / 4
                  f0 = f0 / syntonic_comma ** j
               end if

               j = nint(rational(next(numeral, '-1')))

               if (j .lt. 0.0_dp) then
                  keynote = i
               else
                  f0 = f0 * 2.0_dp ** (j - 4.0_dp)

                  fi = f0; f = fi
               end if

            case ('='); f0 = n() / s; fi = f0; f = fi
            case ('&'); a0 = n();     ai = a0; a = ai
            case ('%'); r0 = n();     ri = r0; r = ri

            case ('V')
               v = n() / s
               f1 = 2 * pi * n() / s
               v1 = cmplx(cos(f1), sin(f1), dp)

            case ('_', '^'); fb = 2.0_dp ** (sgn('_^') * n() / steps)
            case ('\', '/'); fd = 2.0_dp ** (sgn('\/') * n() / steps)
            case ('-', '+'); fi = 2.0_dp ** (sgn('-+') * n() / steps) * f0
               f = fi

            case (',', ';'); ab = 10.0_dp ** (sgn(',;') * n() * 0.1_dp)
            case ('>', '<'); ad = 10.0_dp ** (sgn('><') * n() * 0.1_dp)
            case ('?', '!'); ai = 10.0_dp ** (sgn('?!') * n() * 0.1_dp) * a0
               a = ai

            case ('{', '}'); rb = 10.0_dp ** (sgn('{}') * n() * 0.1_dp)
            case ('(', ')'); rd = 10.0_dp ** (sgn('()') * n() * 0.1_dp)
            case ('[', ']'); ri = 10.0_dp ** (sgn('[]') * n() * 0.1_dp) * r0
               r = ri

            case ("'")
               x = x + rational(next(numeral, '1')) * b
               d = nint(x) - t

               f1 = fd ** (1.0_dp / d) * fb ** (1.0_dp / b); fd = 1.0_dp
               a1 = ad ** (1.0_dp / d) * ab ** (1.0_dp / b); ad = 1.0_dp
               r1 = rd ** (1.0_dp / d) * rb ** (1.0_dp / b); rd = 1.0_dp

               do i = c + 1, c + d
                  phase = phase - floor(phase)

                  rho(i) = a * wave(floor(size(wave) * phase))
                  phi(i) = atan(r)

                  phase = phase + f + real(v, dp)

                  f = f * f1
                  v = v * v1
                  a = a * a1
                  r = r * r1
               end do

               c = c + d
               t = t + d

            case ('"', '`')
               x = x + sgn('`"`') * rational(next(numeral, '1')) * b
               t = nint(x)

            case ('M')
               marks(int(n())) = x

            case ('W')
               x = marks(int(n()))
               t = nint(x)

            case ('P', 'R')
               x1 = marks(int(n()))
               x2 = marks(int(n()))
               dx = x2 - x1

               if (symbol .eq. 'R') then
                  copies = int(n())
               else
                  copies = 1
               end if

               t1 = nint(x1)
               t2 = nint(x2)
               dt = t2 - t1

               do i = 1, copies
                  x = x + dx
                  t = nint(x)
                  mel(:, t - dt + 1:t) = mel(:, t - dt + 1:t) &
                     + mel(:, t1 + 1:t2)
               end do

            case ('I')
               call remember(int(n()))

            case ('J')
               mark = int(n())
               count = int(n())

               call get(info)

               if (info .lt. count) then
                  call set(info + 1)
                  call revert(mark)
               else
                  call set(0)
               end if

            case ('L')
               t1 = nint(marks(int(n())))
               t2 = nint(marks(int(n())))
               dx = n() * s

               factor = n() * size(wave) / (t2 - t1 - 1)

               allocate(work(tones%channels, t2 - t1))

               do i = 0, t2 - t1 - 1
                  work(:, 1 + i) = mel(:, t1 + 1 + i &
                     + nint(dx * wave(modulo(nint(i * factor), size(wave)))))
               end do

               mel(:, t1 + 1:t2) = mel(:, t1 + 1:t2) + work

               deallocate(work)

            case ('*')
               symbol = next('*', length=1)

            case ('none')
               exit
         end select
      end do

      tones%amplitude = maxval(abs(mel))

      if (tones%amplitude .ne. 0.0_dp) mel(:, :) = mel / tones%amplitude

      tones%sound = nint(i2max * mel, i2)

   contains

      function done()
         logical :: done

         done = symbol .eq. 'none'
         done = done .or. scan(symbol, initial) .gt. 0
         done = done .and. c .gt. 0
      end function done

      function n()
         real(dp) :: n

         n = rational(next(numeral))
      end function n

      function sgn(minusplus)
         integer :: sgn

         character(2), intent(in) :: minusplus

         sgn = 2 * index(minusplus, symbol) - 3
      end function sgn
   end subroutine play

   subroutine load(x, what, how, i)
      real(dp), intent(out), allocatable :: x(:)
      character(*), intent(in) :: what, how
      integer, intent(in) :: i
      type(audio) :: s

      if (how .eq. '#') then
         if (i .gt. command_argument_count() - 2) then
            write (stderr, "('Error: file ', I0, ' missing.')") i
            stop
         end if

         call read_riff(command_argument(i, '/dev/stdin'), s)

         allocate(x(0:s%points - 1))

         x(:) = s%sound(0, :)
         x(:) = s%amplitude / i2max * x
      else
         allocate(x(0:i - 1))

         call sample(x, what, how)
      end if
   end subroutine load
end module interpreter
