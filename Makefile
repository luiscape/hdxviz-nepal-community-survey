#
#  PROGRAM INSTRUCTIONS: -----------------------------
#
#  This Makefile contains the following instructions
#
#    - setup: installs necessary dependencies.
#    - test: runs the application unit tests.
#    - run: starts the application.
#
#  ---------------------------------------------------
#

setup:
	bash bin/setup.sh;

test:
	bash bin/testh.sh;

run:
	bash bin/run.sh;
