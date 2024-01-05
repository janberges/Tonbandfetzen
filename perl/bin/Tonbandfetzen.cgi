#!/usr/bin/perl

BEGIN { chdir $0 =~ s|[^/]*$||r }

use Tonbandfetzen;

use CGI;
use CGI::Carp "fatalsToBrowser";
$I = new CGI->param("I");

-e || mkdir $_ and chdir $_ || die for "kurzweiliges";

opendir DIR, ".";
/^\./ or -M > 1 and unlink for readdir DIR;
closedir DIR;

$i = mel $I;

print "Content-type: text/html

<html>
	<head>
		<title>Tonbandfetzen</title>
		<meta http-equiv='content-type' content='text/html; charset=utf-8'>
		<style type='text/css'>
			body {
				margin: 1cm;
				color: #ff8;
				}
			body, textarea {
				font: 12px monospace;
				background-color: #000;
				}
			textarea {
				margin: 0;
				border: 0;
				padding: 0;
				color: #fff;
				}
			a {
				text-decoration: none;
				}
			a:link, a:visited {
				color: #8ff;
				}
			a:active, a:focus, a:hover {
				color: #f8f;
				}
		</style>
	</head>
	<body>
		K A N   N S T   # D U   #       I N #   D E N       #   B Ä U   M E N   # D I   E # T   O N B   A N D<br>
		  F     E   T   Z   E   N       #   S   E   H       E   N       ?         #         W   E       R   #<br>
		  H     A   T   #   S   I E #   D O R   T   H   I N #   G E B   R A C     H     T ? #   A L L   E   S<br>
		  #     G   E   H   Ö   R   T   #   D   I   R   ,   #   E       I         N     E       #       W   E<br>
		  L     T # A   U   S   # P A   P   I   E   R   . # V   O       N # T     O     C O T   R O N   I   C<br>
		<br>";

if (@$i) {
	make "$$.aif", fix $i;

	for ($I) {
		s/&/&amp;/g;
		s/</&lt;/g;
		s/>/&gt;/g;
		}

	print "
		<a href='kurzweiliges/$$.aif'>
			K   E   I N E   # M E   I   S   T E R   W E R   K E #   M E H   R       ! # D       I   E # Z   E I T<br>
			#   I   S       T       #   L   Ä   N     G     S       T       #       S   C       H   O       N   #<br>
			R E I   F # D   A       F   Ü   R   !     #     W A S   #       W       I R #   N I E   M A L   S   #<br>
			Z   U   #       E       N   D   E   #     B     R       I       N       G   E   N   #   K       A   N<br>
			N   #   K E I   N       # M O   L   O     C     H # J   E       # V E   R   S   C H L   I N G   E   N<br>
		</a>
		<br>";
	}

print "
		<form action='' method='post' accept-charset='utf-8'>
			<textarea name='I' cols='101' rows='27'>", $I || "C2 [30 /60 }60 2 \\60 {60 2", "</textarea><br>
			<input type='submit'>
		</form>
	</body>
</html>";
