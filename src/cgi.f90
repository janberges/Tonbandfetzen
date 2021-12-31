! For standalone use on server, compile with FFLAGS='-static -O3'.
! Syntax highlighting in textarea inspired by Will Boyd's article:
! https://codersblock.com/blog/highlight-text-inside-a-textarea/

program mel_dot_cgi
   use constants
   use interpreter
   use io
   use riff
   implicit none

   type(audio) :: music

   character(:), allocatable :: query
   integer, parameter :: limit = 1000000

   query = decode(environment_variable('QUERY_STRING'))

   if (query .eq. ' ') then
      write (*, '(A, /, 1000(:, /, A))') "Content-type: text/html", &
         "<!DOCTYPE html>", &
         "<html lang='en'>", &
         "  <head>", &
         "    <meta charset='utf-8'>", &
         "    <title>Tonbandfetzen</title>", &
         "    <link rel='icon' type='image/svg+xml' sizes='any'", &
         "      href='https://raw.githubusercontent.com/janberges/&
                  &Tonbandfetzen/master/logo/logo.svg'>", &
         "    <style>", &
         "      body { background: #222222 }", &
         "      #color {", &
         "        position: absolute;", &
         "        color: #ffffff;", &
         "        background: #333333;", &
         "      }", &
         "      #mel {", &
         "        position: relative;", &
         "        color: transparent;", &
         "        background: transparent;", &
         "        caret-color: #ffffff;", &
         "      }", &
         "      #color, #mel, #play, #wav {", &
         "        display: block;", &
         "        box-sizing: border-box;", &
         "        width: 600px;", &
         "      }", &
         "      #color, #mel {", &
         "        height: 300px;", &
         "        resize: none;", &
         "        overflow-y: auto;", &
         "        padding: 5px;", &
         "        border: 0;", &
         "        margin: 0;", &
         "        font: bold 16px monospace;", &
         "        white-space: pre-wrap;", &
         "        word-wrap: break-word;", &
         "      }", &
         "      #play { margin-bottom: 5mm }", &
         "      .N { color: #bf8040 }", &
         "      .L { color: #afdf00 }", &
         "      .C { color: #da193b }", &
         "    </style>", &
         "    <script>", &
         "      function enter() {", &
         "        document.getElementById('play').disabled = false", &
         "        var m = document.getElementById('mel').value", &
         "        m = m.replace(/&/g, '&AMP;')", &
         "        m = m.replace(/</g, '&LT;')", &
         "        m = m.replace(/[\d.:]+/g, '<SPAN CLASS=""N"">$&</SPAN>')", &
         "        m = m.replace(/[a-z#]+/g, '<SPAN CLASS=""L"">$&</SPAN>')", &
         "        m = m.replace(/\n$/g, '$&&nbsp;')", &
         "        m = m.replace(/\*[^*]*\*?/g, function(c) {", &
         "          c = c.replace(/<.+?>/g, '')", &
         "          return '<SPAN CLASS=""C"">' + c + '</SPAN>'", &
         "        })", &
         "        document.getElementById('color').innerHTML = m", &
         "      }", &
         "      function move() {", &
         "        document.getElementById('color').scrollTop =", &
         "          document.getElementById('mel').scrollTop", &
         "      }", &
         "      function play() {", &
         "        document.getElementById('play').disabled = true", &
         "        document.getElementById('wav').src = '?'", &
         "          + encodeURIComponent(", &
         "            document.getElementById('mel').value)", &
         "        document.getElementById('wav').load()", &
         "      }", &
         "    </script>", &
         "  </head>", &
         "  <body onload='enter()'>", &
         "    <div id='color'></div>", &
         "    <textarea id='mel' spellcheck='false'", &
         "      oninput='enter()' onscroll='move()'>", &
         "$22050", &
         "*Harmonic series*", &
         "T pyth", &
         "M A2'8", &
         "W ,5 A2' A3' E4' A4' C#v5' E5' Gz5' A5'", &
         "</textarea>", &
         "    <button id='play' onclick='play()'>Interpret</button>", &
         "    <audio id='wav' controls autoplay>Sorry</audio>", &
         "  </body>", &
         "</html>"
   else
      call play(query, music, limit)
      if (music%points .eq. 0) call play("$22050 |1:6 E3' C3'", music)

      music%amplitude = 1.0_dp

      call write_riff('http', music)
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
end program mel_dot_cgi
