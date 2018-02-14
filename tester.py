import sys
import os
import subprocess
from subprocess import run, PIPE

if len(sys.argv) == 2:
	racket_binary = sys.argv[1]
	start = 1
	end = 28
elif len(sys.argv) == 3:
	racket_binary = sys.argv[1]
	start = int(sys.argv[2])
	end = start
elif len(sys.argv) == 4:
	racket_binary = sys.argv[1]
	start = int(sys.argv[2])
	end = int(sys.argv[3])
else:
	sys.exit(1)

programs = range(start, end + 1)

print(f'Testing programs {programs}')

def check(program, input, stdout, expected):
	parts = stdout.split('\n')
	if len(parts) > 1:
		actual = parts[1][4:]
		if actual == expected:
			print(f'Test passed for program {program}')
		else:
			print(f'Test failure for program {program}')
			print(f'Got "{actual}" expected "{expected}"')
			print(f'Input: "{input}"')
			print(stdout)
	else:
		print(f'Test failure for program {program}')
		print(stdout)

for program in programs:
	program_file = os.path.join('test', os.path.join('programs', str(program)))
	expected_file = os.path.join('test', os.path.join('expected', str(program)))

	expected = open(expected_file, 'r').read().strip()

	rkt_string = f'(require "interpreter.rkt") (interpret-test-test "test/programs/{program}")'

	# print(f'Piping {rkt_string} to {racket_binary}')

	process = run([racket_binary], stdout=PIPE, input=rkt_string, encoding='ascii')

	check(program, rkt_string, process.stdout, expected)

