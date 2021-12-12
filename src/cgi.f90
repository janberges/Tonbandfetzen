! For standalone use on server, compile with FFLAGS='-static -O3'.

program cgi
   use constants
   use interpreter
   use io
   use riff
   implicit none

   type(audio) :: music

   character(:), allocatable :: query
   character(*), parameter :: example &
      = "$22050 T pyth M A2'8 W ,5 A2' A3' E4' A4' C#v5' E5' Gz5' A5'"
   integer, parameter :: limit = 1000000

   query = decode(environment_variable('QUERY_STRING'))

   if (query .eq. ' ') then
      write (*, '(A, /, 38(/, A))') "Content-type: text/html", &
         "<!DOCTYPE html>", &
         "<html>", &
         "  <head>", &
         "    <title>Tonbandfetzen</title>", &
         "    <style type='text/css'>", &
         "      body {", &
         "        font-family: 'Liberation Sans', Helvetica, sans-serif", &
         "      }", &
         "      textarea, button, audio {", &
         "        display: block;", &
         "        box-sizing: border-box;", &
         "        width: 500px;", &
         "      }", &
         "    </style>", &
         "    <script type='text/javascript'>", &
         "      function play() {", &
         "        var mel = document.getElementById('mel')", &
         "        var wav = document.getElementById('wav')", &
         "        wav.src = '?' + encodeURIComponent(mel.value)", &
         "        var ctrl = document.getElementById('ctrl')", &
         "        ctrl.load()", &
         "        ctrl.play()", &
         "      }", &
         "    </script>", &
         "  </head>", &
         "  <body>", &
         "    <h1>Tonbandfetzen</h1>", &
         "    <h2>Input</h2>", &
         "    <textarea id='mel' rows='10'>", example, "</textarea>", &
         "    <button onclick='play()'>Play</button>", &
         "    <h2>Output</h2>", &
         "    <audio id='ctrl' controls>", &
         "      <source id='wav' type='audio/wave'>", &
         "    </audio>", &
         "  </body>", &
         "</html>"
   else
      call play(query, music, limit)
      if (music%points .eq. 0) call play("|1:6 E3' C3'", music)

      music%amplitude = 1.0_dp

      write (*, "('Content-type: audio/wav', /)")
      call write_riff('stdout', music)
   end if

contains

   function decode(code) result(url)
      character(:), allocatable :: url

      character(*), intent(in) :: code

      integer :: i, n

      url = code

      i = 1
      do
         n = index(url(i:), '%')
         if (n .eq. 0) exit
         i = n + i

         read (url(i:i + 1), '(Z2)') n
         url = url(:i - 2) // char(n) // url(i + 2:)
      end do
   end function decode
end program cgi
