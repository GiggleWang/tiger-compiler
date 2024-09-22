.PHONY: docker-build docker-pull docker-run docker-run-backend transform build gradelab1 gradelab2 gradelab3 gradelab4 gradelab5 gradelab6 gradeall clean register format


IMAGE_NAME=ipadsse302/tigerlabs_env
IMAGE_VERSION=2.0.0
IMAGE_NAME_FULL=$(IMAGE_NAME):$(IMAGE_VERSION)
URL = https://ipads.se.sjtu.edu.cn/courses/compilers/labs/tigerlabs_env_v2.tar
IMAGE_FILE = tigerlabs_env_v2.tar

docker-build:
	docker build -t $(IMAGE_NAME_FULL) .

docker-pull:
	docker pull $(IMAGE_NAME_FULL)

check-image:
	@if ! docker images | grep $(IMAGE_NAME) | grep -q $(IMAGE_VERSION); then \
		echo "Image $(IMAGE_NAME_FULL) not found. Pulling..."; \
		curl -o $(IMAGE_FILE) $(URL); \
		docker load -i $(IMAGE_FILE); \
	else \
		echo "Image $(IMAGE_NAME_FULL) exists."; \
	fi


docker-run: check-image
	docker run --platform linux/amd64 -it --privileged --dns=223.5.5.5 \
		-v $(shell pwd):/home/stu/tiger-compiler $(IMAGE_NAME_FULL)
	

docker-run-backend: check-image
	docker run --platform linux/amd64 -dt --privileged --dns=223.5.5.5 \
		-v $(shell pwd):/home/stu/tiger-compiler $(IMAGE_NAME_FULL)

transform:
	find src scripts testdata -type f | xargs -I % sh -c 'dos2unix -n % /tmp/tmp; mv -f /tmp/tmp % || true;'

build:transform
	mkdir -p build && cd build && cmake -DCMAKE_BUILD_TYPE=Release .. && make

build-debug:transform
	mkdir -p build && cd build && cmake -DCMAKE_BUILD_TYPE=Debug .. && make

gradelab1:transform
	bash scripts/grade.sh lab1

gradelab2:transform
	bash scripts/grade.sh lab2

gradelab3:transform
	bash scripts/grade.sh lab3

gradelab4:transform
	bash scripts/grade.sh lab4

gradelab5-1:transform
	bash scripts/grade.sh lab5-part1

gradelab5:transform
	bash scripts/grade.sh lab5

gradelab6:transform
	bash scripts/grade.sh lab6

gradeall:transform
	bash scripts/grade.sh all

clean:
	rm -rf build/ src/tiger/lex/scannerbase.h src/tiger/lex/lex.cc \
		src/tiger/parse/parserbase.h src/tiger/parse/parse.cc

register:
	python3 scripts/register.py

format:
	find . \( -name "*.h" -o -iname "*.cc" \) | xargs clang-format -i -style=file

