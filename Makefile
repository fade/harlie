SBCL        := sbcl
BUILD_SCRIPT := build.lisp
TARGET       := consort
DEPLOY_DIR   := /home/glenn/consort

.PHONY: all build clean deploy

all: build

build: $(TARGET)

$(TARGET): $(BUILD_SCRIPT) harlie.asd $(wildcard *.lisp)
	$(SBCL) --dynamic-space-size 1024 --non-interactive --load $(BUILD_SCRIPT)

clean:
	rm -f $(TARGET)
	rm -rf ~/.cache/common-lisp/sbcl-*/**/harlie/

deploy: $(TARGET)
	install -d $(DEPLOY_DIR)
	install -m 755 $(TARGET) $(DEPLOY_DIR)/$(TARGET)
