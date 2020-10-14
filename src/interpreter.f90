module interpreter
   use constants
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

      character(*), parameter :: &
         numeral = '0123456789.:', &
         lexical = 'abcdefghijklmnopqrstuvwxyz', &
         special = '~"`ABCDEFGNSZ=-+&?!%[]\/><()_^,;{}$*|@#''', &
         initial = special(:22)

      character(:), allocatable :: symbol, word ! special/lexical string

      real(dp), allocatable ::    & ! sound stages:
         wave(:),                 & !   I. samples
         rise(:), rho(:),         & !  II. segments (polar coordinates)
         fall(:), phi(:), mel(:, :) ! III. melody (channel-wise)

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

      integer :: i ! arbitrary integer/index

      real(dp) :: s ! equivalent of a second

      tones%rate = 44100.0_dp
      tones%channels = 2

      todo = .true.

      call focus(notes)

      do
         select case (next(special))
            case ('$'); tones%rate = n()
            case ('*'); tones%channels = int(n(), i2)

            case ('~'); todo( 1 ) = .false.
            case ('S'); todo( 2 ) = .false.
            case ('Z'); todo( 3 ) = .false.
            case ('N'); todo(2:3) = .false.

            case ("'", 'none')
               exit
         end select
      end do

      s = tones%rate

      if (todo(1)) call load(wave, 'loop', 'circular', nint(1.0_dp * s))
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

      do
         symbol = next(special)

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
               x = x + n() * b
               d = nint(x) - t
               c = c + d
               t = t + d

            case ('"', '`')
               x = x + sgn('`"') * n() * b
               t = nint(x)

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

      phase = 0.0_dp

      x = 0.0_dp

      t = -tmin
      c = 0

      call reset

      do
         symbol = next(special)

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
               i = nint(n() * s)

               select case (symbol)
                  case ('~'); call load(wave, 'loop', word, i)
                  case ('S'); call load(rise, 'fade', word, i)
                  case ('Z'); call load(fall, 'fade', word, i)
                  case ('N'); call load(rise, 'fade', word, i)
                     fall = rise
               end select

            case ('|')
               b = n() * s

            case ('@')
               A4 = n() / s

            case ('#')
               steps = nint(n())

            case ('C','C#','D','D#','E','F','F#','G','G#','A','A#','B')
               i = index('C#D#EF#G#A#B', symbol) + len(symbol) - 11
               f0 = A4 * 2.0_dp ** (n() - 4.0_dp + i / 12.0_dp)
               fi = f0; f = fi

            case ('='); f0 = n() / s; fi = f0; f = fi
            case ('&'); a0 = n();     ai = a0; a = ai
            case ('%'); r0 = n();     ri = r0; r = ri

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
               x = x + n() * b
               d = nint(x) - t

               f1 = fd ** (1.0_dp / d) * fb ** (1.0_dp / b); fd = 1.0_dp
               a1 = ad ** (1.0_dp / d) * ab ** (1.0_dp / b); ad = 1.0_dp
               r1 = rd ** (1.0_dp / d) * rb ** (1.0_dp / b); rd = 1.0_dp

               do i = c + 1, c + d
                  phase = phase - floor(phase)

                  rho(i) = a * wave(floor(size(wave) * phase))
                  phi(i) = atan(r)

                  phase = phase + f

                  f = f * f1
                  a = a * a1
                  r = r * r1
               end do

               c = c + d
               t = t + d

            case ('"', '`')
               x = x + sgn('`"`') * n() * b
               t = nint(x)

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

   subroutine load(x, what, how, length)
      real(dp), intent(out), allocatable :: x(:)
      character(*), intent(in) :: what, how
      integer, intent(in) :: length
      type(audio) :: s

      if (what .eq. 'loop' .and. length .eq. 0) then
         call read_riff(how // '.wav', s)

         x = s%sound(0, :)
         x = s%amplitude / i2max * x
      else
         allocate(x(0:length - 1))

         call sample(x, what, how)
      end if
   end subroutine load
end module interpreter
