
TARGET = ./cabal/bin/push-swap
XHOST = host.docker.internal
XDISPLAY = $(XHOST):0.0

.PHONY: all clean build run install

show: $(TARGET)
	DISPLAY=$(XDISPLAY) python3 vis/pyviz.py `./cabal/bin/shuffled 100`

small: $(TARGET)
	DISPLAY=$(XDISPLAY) python3 vis/pyviz.py `./cabal/bin/shuffled 20`

build:
	sudo docker-compose run ghc cabal build

$(TARGET): install

install: build
	sudo docker-compose run ghc cabal install --overwrite-policy=always
