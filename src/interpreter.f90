module interpreter
   use constants, only: audio, dp, i2, i2max, stderr
   use fjs, only: accidentals, comma
   use io, only: command_argument
   use lcg, only: minstd
   use rationals, only: rational
   use riff, only: read_riff
   use samples, only: sample
   use search, only: focus, get, known, lexical, next, &
      numeral, remember, reset, revert, set, special
   implicit none
   private

   public :: play

contains

   subroutine play(notes, tones, limit)
      character(*), intent(in) :: notes
      type(audio), intent(out) :: tones
      integer, intent(in), optional :: limit

      character(*), parameter :: initial = '!"%&+-=?@ABCDEFGNQSUVWXYZ[]`~'

      character(:), allocatable :: symbol, word ! special/lexical string

      real(dp), allocatable :: wave(:), rise(:), fall(:) ! sound samples
      real(dp), allocatable :: rho(:), phi(:) ! sound segment
      real(dp), allocatable :: mel(:, :) ! melody
      real(dp), allocatable :: work(:, :) ! temporary data

      logical :: todo(3) ! samples yet to be initialized?
      logical :: over ! end of input reached?

      real(dp) :: x ! exact time
      real(dp) :: b ! beat duration

      integer :: t ! rounded time
      integer :: c ! continuance
      integer :: p ! processed time
      integer :: d ! note duration

      integer :: tmin, tmax, cmax ! extreme times

      real(dp) :: A4 ! concert pitch

      integer :: steps ! notes per octave

      real(dp) :: f ! frequency f(t)
      real(dp) :: f0 ! reference frequency
      real(dp) :: fi ! initial frequency
      real(dp) :: fd ! f(t + d) / f(t)
      real(dp) :: fb ! f(t + b) / f(t)
      real(dp) :: f1 ! f(t + 1) / f(t)

      real(dp) :: a ! amplitude a(t) = sqrt(L^2 + R^2)
      real(dp) :: a0 ! reference amplitude
      real(dp) :: ai ! initial amplitude
      real(dp) :: ad ! a(t + d) / a(t)
      real(dp) :: ab ! a(t + b) / a(t)
      real(dp) :: a1 ! a(t + 1) / a(t)

      real(dp) :: r ! amplitudes ratio r(t) = R:L
      real(dp) :: r0 ! reference ratio
      real(dp) :: ri ! initial ratio
      real(dp) :: rd ! r(t + d) / r(t)
      real(dp) :: rb ! r(t + b) / r(t)
      real(dp) :: r1 ! r(t + 1) / r(t)

      real(dp) :: phase ! turn = 1

      integer :: i, j, k ! arbitrary integers/indices
      logical :: l ! arbitrary logical

      real(dp) :: s ! equivalent of a second

      ! time marks
      real(dp) :: marks(0:99)
      logical :: mark_set(0:99)
      real(dp) :: x1, x2, dx
      integer :: t1, t2, dt

      real(dp) :: random, factor

      ! tuning
      character(:), allocatable :: tuning

      real(dp), parameter :: equal_fifth = 2.0_dp ** (7.0_dp / 12.0_dp)
      real(dp), parameter :: just_fifth = 1.5_dp

      integer :: keynote, tone, newtone

      integer, allocatable :: primes(:)

      integer :: keycount(-25:23)

      tuning = 'equal'
      tone = 0
      keynote = -3 ! C

      keycount = 0

      tones%rate = 44100.0_dp
      tones%channels = -1_i2

      todo = .true.

      call focus(notes)

      do
         select case (next(special, length=1))
         case ('$')
            tones%rate = n()

         case ('~')
            todo(1) = .false.
         case ('S')
            todo(2) = .false.
         case ('Z')
            todo(3) = .false.
         case ('N')
            todo(2:3) = .false.

         case ('*')
            if (next('*', length=1, barrier='*') .eq. 'none') exit

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
      p = 0

      tmin = t
      tmax = t
      cmax = c

      call reset
      mark_set = .false.

      do
         symbol = next(special, length=1)

         tmin = min(tmin, t)
         tmax = max(tmax, t)

         if (done()) then
            cmax = max(cmax, c)
            c = 0
         end if

         select case (symbol)
         case ('~', 'S', 'Z', 'N')
            if (next(lexical) .ne. '#') then
               p = p + nint(n(1.0_dp) * s)
            end if

         case ('O')
            tones%channels = int(n(), i2)

         case ('%', '(', ')', '[', ']', '{', '}')
            if (tones%channels .eq. -1_i2) tones%channels = 2_i2

         case ("'")
            x = x + n(1.0_dp) * b
            d = nint(x) - t
            c = c + d
            t = t + d
            p = p + d

         case default
            call routine_cases
            if (over) exit
         end select
      end do

      allocate(phi(cmax))
      allocate(rho(cmax))

      tones%points = tmax - tmin

      if (tones%channels .eq. -1_i2) tones%channels = 1_i2

      if (present(limit)) then
         if (p .gt. limit .or. tones%points .gt. limit) tones%points = 0
      end if

      allocate(mel(tones%channels, tones%points))

      if (tones%points .eq. 0) return

      mel = 0.0_dp

      b = 0.5_dp * s
      A4 = 440.0_dp / s
      steps = 12

      f0 = A4
      f = f0
      fi = f0
      fd = 1.0_dp
      fb = 1.0_dp

      a0 = 1.0_dp
      a = a0
      ai = a0
      ad = 1.0_dp
      ab = 1.0_dp

      r0 = 1.0_dp
      r = r0
      ri = r0
      rd = 1.0_dp
      rb = 1.0_dp

      phase = 0.0_dp

      x = 0.0_dp

      t = -tmin
      c = 0

      call reset
      mark_set = .false.

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
            case (1_i2)
               mel(1, i:t) = mel(1, i:t) + rho(:c)

            case (2_i2)
               mel(1, i:t) = mel(1, i:t) + rho(:c) * cos(phi(:c))
               mel(2, i:t) = mel(2, i:t) + rho(:c) * sin(phi(:c))
            end select

            c = 0
         end if

         select case (symbol)
         case ('~', 'S', 'Z', 'N')
            word = next(lexical)

            if (word .eq. '#') then
               i = int(n(1.0_dp))
            else
               i = nint(n(1.0_dp) * s)
            end if

            select case (symbol)
            case ('~')
               call load(wave, 'wave', word, i)
            case ('S')
               call load(rise, 'fade', word, i)
            case ('Z')
               call load(fall, 'fade', word, i)
            case ('N')
               call load(rise, 'fade', word, i)
               fall = rise
            end select

         case ('X')
            word = next(lexical)

            select case (word)
            case ('status')
               write (stderr, "(*(A10, ':', F16.9, 1X, A, :, /))") &
                  'Time', t / s, 's', &
                  'Frequency', f * s, 'Hz', &
                  'Amplitude', a, 'arb. units', &
                  'Balance', 10 * log10(r), 'dB', &
                  'Phase', phase, '(mod 1)'

            case ('report')
               write (stderr, "('Note counts:')")

               do i = lbound(keycount, 1), ubound(keycount, 1)
                  if (keycount(i) .gt. 0) then
                     j = modulo(i + 4, 7) + 1

                     write (stderr, "(A)", advance='no') 'FCGDAEB'(j:j)

                     j = (i + 4 - (j - 1)) / 7

                     if (j < 0) write (stderr, "(A)", advance='no') &
                        repeat('b', -j)

                     if (j > 0) write (stderr, "(A)", advance='no') &
                        repeat('#', j)

                     write (stderr, "(': ', I0)") keycount(i)
                  end if
               end do

               keycount = 0

            case ('detune')
               call minstd(random)
               random = 1.0_dp - 2.0_dp * random
               random = 2.0_dp ** (random * n() / steps)
               A4 = A4 * random
               f0 = f0 * random
               fi = fi * random
               f = f * random

            case ('delete', 'reverse', 'flanger', 'vibrato')
               i = int(n())
               j = int(n())

               if (mark_set(i) .and. mark_set(j)) then
                  t1 = nint(marks(i))
                  t2 = nint(marks(j))

                  select case (word)
                  case ('delete')
                     mel(:, t1 + 1:t2) = 0.0_dp

                  case ('reverse')
                     mel(:, t1 + 1:t2) = mel(:, t2:t1 + 1:-1)

                  case ('flanger', 'vibrato')
                     dx = n() * s

                     factor = n() * size(wave) / (t2 - t1 - 1)

                     allocate(work(tones%channels, t2 - t1))

                     do i = 0, t2 - t1 - 1
                        work(:, 1 + i) = mel(:, t1 + 1 + i + nint(dx * &
                           wave(modulo(nint(i * factor), size(wave)))))
                     end do

                     select case (word)
                     case ('flanger')
                        mel(:, t1 + 1:t2) = mel(:, t1 + 1:t2) + work
                     case ('vibrato')
                        mel(:, t1 + 1:t2) = work
                     end select

                     deallocate(work)
                  end select
               end if

            case default
               write (stderr, "('Warning: Unknown action ''', A, '''.')") word
            end select

         case ('T')
            tuning = next(lexical)

            select case (tuning)
            case ('equal', 'pyth', 'just', 'close')
               continue
            case default
               write (stderr, "('Warning: Unknown tuning ''', A, '''.')") tuning
               write (stderr, "('The tuning ''equal'' is used instead.')")
               write (stderr, "('See ''man mel'' for list of tunings.')")
               tuning = 'equal'
            end select

         case ('H')
            steps = nint(n())

         case ('C', 'D', 'E', 'F', 'G', 'A', 'B', 'U', 'V')
            f = A4

            if (index('UV', symbol) .ne. 0) then
               newtone = tone + sgn('VU') * int(n())
               i = keynote - 5 + modulo(newtone * 7 - keynote + 5, 12)
            else
               i = index('FCGDAEB', symbol) - 5
            end if

            word = next(lexical, '')

            do j = 1, len(word)
               select case (word(j:j))
               case ('b')
                  i = i - 7
               case ('#')
                  i = i + 7
               case ('x')
                  i = i + 14
               case ('v') ! syntonic comma down
                  f = f * comma(5)
               case ('u') ! syntonic comma up
                  f = f * comma(-5)
               case ('z') ! septimal comma down
                  f = f * comma(7)
               case ('s') ! septimal comma up
                  f = f * comma(-7)
               case ('j') ! 11-comma down
                  f = f * comma(-11)
               case ('i') ! 11-comma up
                  f = f * comma(11)
               case ('d') ! ditonic comma down
                  f = f * comma(-3)
               case ('p') ! Pythagorean comma up
                  f = f * comma(3)
            end select
            end do

            if (lbound(keycount, 1) .le. i .and. &
                ubound(keycount, 1) .ge. i) then
               keycount(i) = keycount(i) + 1
            end if

            select case (tuning)
            case ('equal')
               f = f * equal_fifth ** i

            case ('pyth')
               f = f * just_fifth ** i

            case ('just')
               f = f * just_fifth ** i
               j = i + modulo(1 - keynote, 4)
               j = (j - modulo(j, 4)) / 4
               f = f * comma(5) ** j

            case ('close')
               f = f * just_fifth ** i
               j = i + modulo(5 - keynote, 11)
               j = (j - modulo(j, 11)) / 11
               f = f * comma(5) ** j
            end select

            ! fold back to first octave:
            j = 4 * i + 5
            j = (j - modulo(j, 7)) / 7
            f = f / 2.0_dp ** j

            ! position on twelve-tone scale:
            j = i * 7 - 12 * j

            if (index('UV', symbol) .ne. 0) then
               j = (newtone - j) / 12
            else
               word = next(numeral, 'none')

               tone = j

               if (word .eq. 'none') then
                  j = 4
                  keynote = i
               else
                  j = nint(rational(word))
               end if

               tone = tone + 12 * j
            end if

            f = f * 2.0_dp ** (j - 4.0_dp)
            fi = f

            word = next(numeral, 'none')

            if (word .ne. 'none') then
               primes = accidentals(word)
               do j = 1, size(primes)
                  f = f * comma(primes(j))
               end do
            end if

            if (index('UV', symbol) .eq. 0) f0 = f

         case ('@')
            A4 = n(s / size(wave)) / s
            f0 = A4
            fi = f0
            f = fi

         case ('&')
            a0 = n()
            ai = a0
            a = ai

         case ('%')
            r0 = n()
            ri = r0
            r = ri

         case ('R')
            f0 = f
            fi = f0
            a0 = a
            ai = a0
            r0 = r
            ri = r0

         case ('Q')
            fi = n() * f0
            f = fi

         case ('_', '^')
            fb = 2.0_dp ** (sgn('_^') * n() / steps)
         case ('\', '/')
            fd = 2.0_dp ** (sgn('\/') * n() / steps)
         case ('-', '+')
            fi = 2.0_dp ** (sgn('-+') * n() / steps) * f0
            f = fi

         case (',', ';')
            ab = 10.0_dp ** (sgn(',;') * n() * 0.1_dp)
         case ('>', '<')
            ad = 10.0_dp ** (sgn('><') * n() * 0.1_dp)
         case ('?', '!')
            ai = 10.0_dp ** (sgn('?!') * n() * 0.1_dp) * a0
            a = ai

         case ('{', '}')
            rb = 10.0_dp ** (sgn('{}') * n() * 0.1_dp)
         case ('(', ')')
            rd = 10.0_dp ** (sgn('()') * n() * 0.1_dp)
         case ('[', ']')
            ri = 10.0_dp ** (sgn('[]') * n() * 0.1_dp) * r0
            r = ri

         case ('P')
            phase = n()

         case ("'")
            x = x + n(1.0_dp) * b
            d = nint(x) - t

            f1 = fd ** (1.0_dp / d) * fb ** (1.0_dp / b)
            fd = 1.0_dp
            a1 = ad ** (1.0_dp / d) * ab ** (1.0_dp / b)
            ad = 1.0_dp
            r1 = rd ** (1.0_dp / d) * rb ** (1.0_dp / b)
            rd = 1.0_dp

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

         case default
            call routine_cases
            if (over) exit
         end select
      end do

      tones%amplitude = maxval(abs(mel))

      if (tones%amplitude .ne. 0.0_dp) mel = mel / tones%amplitude

      if (tones%channels .eq. 2_i2) then
         tones%amplitude = sqrt(2.0_dp) * tones%amplitude
      end if

      tones%sound = nint(i2max * mel, i2)

   contains

      function done()
         logical :: done

         done = symbol .eq. 'none'
         done = done .or. scan(symbol, initial) .gt. 0
         done = done .and. c .gt. 0
      end function done

      function n(def)
         real(dp) :: n

         real(dp), intent(in), optional :: def

         if (present(def)) then
            n = rational(next(numeral, '-1'))
            if (n .eq. -1.0_dp) n = def
         else
            n = rational(next(numeral))
         end if
      end function n

      function sgn(minusplus)
         integer :: sgn

         character(2), intent(in) :: minusplus

         sgn = 2 * index(minusplus, symbol) - 3
      end function sgn

      subroutine routine_cases
         over = .false.

         select case (symbol)
         case ('|')
            b = n() * s

         case ('"', '`')
            x = x + sgn('`"') * n(1.0_dp) * b
            t = nint(x)

         case ('M')
            i = int(n(0.0_dp))
            marks(i) = x
            mark_set(i) = .true.

         case ('W')
            i = int(n(0.0_dp))
            if (mark_set(i)) then
               x = marks(i)
               t = nint(x)
            end if

         case ('Y')
            i = int(n())
            j = int(n())

            if (mark_set(i) .and. mark_set(j)) then
               x1 = marks(i)
               x2 = marks(j)
               dx = x2 - x1

               if (allocated(mel)) then
                  t1 = nint(x1)
                  t2 = nint(x2)
                  dt = t2 - t1
               end if

               do i = 1, int(n(1.0_dp))
                  x = x + dx
                  t = nint(x)

                  if (allocated(mel)) mel(:, t - dt + 1:t) &
                     = mel(:, t - dt + 1:t) + mel(:, t1 + 1:t2)
               end do
            end if

         case ('I')
            call remember(int(n(0.0_dp)))

         case ('J')
            i = int(n(0.0_dp))

            if (known(i)) then
               j = int(n(1.0_dp))

               call get(k)

               if (k .lt. j) then
                  call set(k + 1)
                  call revert(i)
               else
                  call set(0)
               end if
            end if

         case ('K', 'L')
            call get(i)
            i = i + 1
            call set(i)

            do
               j = int(n(-1.0_dp))
               l = i .eq. j
               if (l .or. j .eq. -1) exit
            end do

            if (symbol .eq. 'K' .eqv. l) then
               do
                  if (next(special, length=1) .ne. '*') return
                  if (next('*', length=1, barrier='*') .eq. 'none') exit
               end do

               over = .true.
            end if

         case ('*')
            if (next('*', length=1, barrier='*') .eq. 'none') over = .true.

         case ('none')
            over = .true.
         end select
      end subroutine routine_cases
   end subroutine play

   subroutine load(x, what, how, i)
      real(dp), intent(out), allocatable :: x(:)
      character(*), intent(in) :: what, how
      integer, intent(in) :: i
      type(audio) :: s

      if (how .eq. '#') then
         if (i .gt. command_argument_count() - 2) then
            write (stderr, "('Error: File ', I0, ' missing.')") i
            stop
         end if

         call read_riff(command_argument(i, '/dev/stdin'), s)

         allocate(x(0:s%points - 1))

         x = s%amplitude / i2max * s%sound(0, :)
      else
         allocate(x(0:i - 1))

         call sample(x, what, how)
      end if
   end subroutine load
end module interpreter
