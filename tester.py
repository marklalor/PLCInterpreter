import sys
import os
import subprocess

if len(sys.argv) == 2:
	racket = sys.argv[1]
	start = 1
	end = 28
elif len(sys.argv) == 3:
	racket = sys.argv[1]
	start = int(sys.argv[2])
	end = start
elif len(sys.argv) == 4:
	racket = sys.argv[1]
	start = int(sys.argv[2])
	end = int(sys.argv[3])
else:
	sys.exit(1)

programs = range(start, end + 1)

for program in programs:
	program_file = os.path.join('test', os.path.join('programs', str(program)))
	expected_file = os.path.join('test', os.path.join('expected', str(program)))

	expected = open(expected_file, 'r').read()

	# subprocess.call([racket, ])

	# TODO: tester


