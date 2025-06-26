;;; Compiled snippets and support files for `makefile-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'makefile-mode
		     '(("golang" "github-org := $1\n########################################################################\nbuildDir := build\nname := $(shell basename `pwd`)\nmainProgram := $(buildDir)/$(name)\n\nentryPoint := cmd/$(name)/$(name).go\nsrcFiles := $(shell find . -name \"*.go\" -not -path \"./$(buildDir)/*\" -not -name \"*_test.go\" -not -path \"*\\#*\")\n\nbuild:$(buildDir)/$(name)\ndist:$(buildDir)/$(name).dist.tar.gz\n\nrun:$(mainProgram)\n	./$< \n\n$(mainProgram):$(entryPoint) $(srcFiles)\n	go build -o $@ $<\n\n$(buildDir)/$(name).dist.tar.gz:lint test build\n	tar -czf $@ --exclude=build --exclude=.git --transform 's,^,$(name)/,' *\n	tar -tf $@\n\ntest:\n	go test -v ./...\nclean:\n	rm $(mainProgram) $(buildDir)/$(name).dist.tar.gz\nlint:\n	@golangci-lint run\n\nsetup: \n	go mod init github.com/$(github-org)/$(name) || true\n	git init\n	mkdir -p $(buildDir)\n	mkdir -p cmd/$(name)\n	if [[ ! -f $(entryPoint) ]]; then echo \"package main\" > $(entryPoint); fi \n	@cat $(entryPoint)\n	if [[ ! -f .gitignore ]]; then echo \"build\" > .gitignore && cat .gitignore; fi \n	@cat .gitignore\n\n.PHONY:build clean test run lint setup\n$0" "golang" nil nil nil "/home/tychoish/.emacs.d/snippets/makefile-mode/golang" nil nil)))


;;; Do not edit! File generated at Mon Dec 30 15:17:58 2024
