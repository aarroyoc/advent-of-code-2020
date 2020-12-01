.PHONY: python
python:
	python3 -m pytest

.PHONY: prolog
prolog:
	swipl -s index.pl -g run_tests,halt -t 'halt(1)'