all: day1.beam
	erl -noshell -s day1 start -s init stop

day1.beam:
	erlc day1.erl

clean:
	rm -f day1.beam
