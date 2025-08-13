;;; "Compiled" snippets and support files for `go-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'go-mode
		     '(("yaml" "\\`yaml:\"$1\"\\`\n" "yaml" nil nil
			nil
			"/home/tychoish/.emacs.d/snippets/go-mode/yaml"
			nil nil)
		       ("xml" "\\`xml:\"$1\"\\`" "xml" nil nil nil
			"/home/tychoish/.emacs.d/snippets/go-mode/xml"
			nil nil)
		       ("wrapf"
			"ers.Wrapf(err, \"${1:template}\", ${2:arguments})\n"
			"wrapf" nil nil nil
			"/home/tychoish/.emacs.d/snippets/go-mode/wrapf"
			nil nil)
		       ("worker"
			"fun.Worker(func(ctx context.Context) error {\n         return nil\n})\n"
			"worker" nil nil nil
			"/home/tychoish/.emacs.d/snippets/go-mode/worker"
			nil nil)
		       ("wfn"
			"func(ctx context.Context) error {\n	$0\n	return nil\n}\n"
			"wfn" nil nil nil
			"/home/tychoish/.emacs.d/snippets/go-mode/wfn"
			nil nil)
		       ("timer"
			"timer := time.NewTimer($1)\ndefer timer.Stop()\n\nselect {\ncase <-ctx.Done():\n	return\ncase <-timer.C:\n	$0\n}\n"
			"timer" nil nil nil
			"/home/tychoish/.emacs.d/snippets/go-mode/timer"
			nil nil)
		       ("tests"
			"tests := []struct {\n	expected\n}{\n	{},\n}\n\nfor n, tt := range tests {\n	actual :=\n	if tt.expected != actual {\n		t.Errorf(\"#%d want %v, got %v\", n, tt.expected, actual)\n	}\n}\n"
			"tests" nil nil nil
			"/home/tychoish/.emacs.d/snippets/go-mode/tests"
			nil nil)
		       ("testf" "func Test$1(t *testing.T) {\n	$0\n}"
			"testf" nil nil nil
			"/home/tychoish/.emacs.d/snippets/go-mode/testf"
			nil nil)
		       ("tags"
			"\\`bson:\"$1\" json:\"$1\" yaml:\"$1\"\\`"
			"tags" nil nil nil
			"/home/tychoish/.emacs.d/snippets/go-mode/tags"
			nil nil)
		       ("t"
			"func Test$1(t *testing.T) {\n	$0\n}\n"
			"test" nil nil nil
			"/home/tychoish/.emacs.d/snippets/go-mode/t"
			nil nil)
		       ("suite"
			"type $1Suite struct {\n	suite.Suite\n}\n\nfunc Test$1Suite(t *testing.T) {\n	suite.Run(t, new($1Suite))\n}\n\nfunc (s *$1Suite) SetupSuite() {}\nfunc (s *$1Suite) TearDownSuite() {}\nfunc (s *$1Suite) SetupTest() {}\nfunc (s *$1Suite) TearDownTest() {}\nfunc (s *$1Suite) Test() {}\n"
			"testify suite" nil nil nil
			"/home/tychoish/.emacs.d/snippets/go-mode/suite"
			nil nil)
		       ("trun"
			"t.Run(\"$1\", func(t *testing.T) {\n	$0\n})\n"
			"subtest" nil nil nil
			"/home/tychoish/.emacs.d/snippets/go-mode/subtest"
			nil nil)
		       ("type" "type $1 struct {\n	$0\n}\n"
			"struct" nil nil nil
			"/home/tychoish/.emacs.d/snippets/go-mode/struct"
			nil nil)
		       ("range"
			"for ${1:key}, ${2:value} := range ${3:target} {\n    $0\n}"
			"range" nil nil nil
			"/home/tychoish/.emacs.d/snippets/go-mode/range"
			nil nil)
		       ("printdebug"
			"var count = &atomic.Int64{}\n\nfunc tychoish(args ...any) {\n	var calling string\n	var padding string\n	pc, _, _, _ := runtime.Caller(1)\n	fnname := runtime.FuncForPC(pc).Name()\n	parts := strings.Split(fnname, \".\")\n	idx := len(parts) - 1\n	if parts[idx] == \"func1\" {\n		idx--\n	}\n	calling = parts[idx]\n	padding = strings.Repeat(\" \", int(math.Max(0, float64(24-len(calling)))))\n\n	fmt.Println(time.Now().UnixMicro(), fmt.Sprintf(\" %03d \", count.Add(1)), calling, padding, args)\n}\n"
			"tychoish print debugger" nil nil nil
			"/home/tychoish/.emacs.d/snippets/go-mode/print-debugger"
			nil nil)
		       ("pkgmain"
			"package main\n\nimport (\n	\"github.com/codegangsta/cli\"\n	\"github.com/tychoish/grip\"\n)\n\nfunc main() {\n	app := cli.NewApp()\n	defer app.RunAndExitOnError()\n\n	app.Name = \"${1:name}\"\napp.Usage = \"${2:usage}\"\n	app.Version = \"0.0.1-pre\"\n\n	app.Commands = []cli.Command{}\n\n	app.Flags = []cli.Flag{\n		cli.StringFlag{\n			Name:  \"level\",\n			Value: \"info\",\n			Usage: \"Specify lowest visable loglevel as string: 'emergency|alert|critical|error|warning|notice|info|debug'\",\n		},\n		cli.BoolFlag{\n			Name:   \"systemdLog\",\n			Usage:  \"log to systemd's journal if possible. defaults to false\",\n			EnvVar: \"GRIP_USE_JOURNALD\",\n		},\n	}\n\n	app.Before = func(c *cli.Context) (err error) {\n		// grip is a systemd/standard logging wrapper.\n		grip.SetName(app.Name)\n\n		if c.Bool(\"systemdLog\") == false {\n			// We're configing the logger to use\n			// standard (output) logging rather than\n			// systemd by default.\n			grip.UseNativeLogger()\n		} else {\n			grip.UseSystemdLogger()\n		}\n\n		grip.SetThreshold(c.String(\"level\"))\n		if grip.ThresholdLevel().String() == \"invalid\" {\n			grip.SetThreshold(\"info\")\n		}\n\n		if grip.ThresholdLevel().String() != \"info\" {\n			grip.Defaultf(\"set log threshold to: '%s'\", grip.ThresholdLevel())\n		}\n		return\n	}\n}\n"
			"pkgmain" nil nil nil
			"/home/tychoish/.emacs.d/snippets/go-mode/pkgmain"
			nil nil)
		       ("pkg"
			"package ${1:`(car (last (split-string (file-name-directory buffer-file-name) \"/\") 2))`}\n"
			"pkg" nil nil nil
			"/home/tychoish/.emacs.d/snippets/go-mode/pkg"
			nil nil)
		       ("pack" "// \npackage ${1:main}\n" "package"
			nil nil nil
			"/home/tychoish/.emacs.d/snippets/go-mode/package"
			nil nil)
		       ("okmap" "if _, ok := $1; ok {\n	$2\n}\n\n$0\n"
			"okmap" nil nil nil
			"/home/tychoish/.emacs.d/snippets/go-mode/okmap"
			nil nil)
		       ("mkstream"
			"fun.MakeStream(func(ctx conext.Context) ($1, error) {\n	$0\n\n        return $2, nil\n})\n"
			"mkstream" nil nil nil
			"/home/tychoish/.emacs.d/snippets/go-mode/mkstream"
			nil nil)
		       ("method"
			"func (${1:target}) ${2:name}(${3:arguments}) (${4:results}) {\n	$0\n}"
			"method" nil nil nil
			"/home/tychoish/.emacs.d/snippets/go-mode/method"
			nil nil)
		       ("lambda" "func ($1) $2 {\n	$0\n}"
			"lambda" nil nil nil
			"/home/tychoish/.emacs.d/snippets/go-mode/lambda"
			nil nil)
		       ("json" "\\`json:\"$1\"\\`" "json" nil nil nil
			"/home/tychoish/.emacs.d/snippets/go-mode/json"
			nil nil)
		       ("type" "type $1 interface {\n	$0\n}\n"
			"interface" nil nil nil
			"/home/tychoish/.emacs.d/snippets/go-mode/interface"
			nil nil)
		       ("imp" "import (\n	\"$1\"\n)" "imp" nil
			nil nil
			"/home/tychoish/.emacs.d/snippets/go-mode/imp"
			nil nil)
		       ("iferr"
			"if err != nil {\n	return err\n}\n"
			"iferr" nil nil nil
			"/home/tychoish/.emacs.d/snippets/go-mode/iferr"
			nil nil)
		       ("handler"
			"func ${1:name}(${2:arguments}) http.HandlerFunc {\n        return func(w http.ResponseWriter, r *http.Request) {\n                $0\n        }\n}\n"
			"handler" nil nil nil
			"/home/tychoish/.emacs.d/snippets/go-mode/handler"
			nil nil)
		       ("forw" "for {\n        $0\n}" "forwhile" nil
			nil nil
			"/home/tychoish/.emacs.d/snippets/go-mode/forw"
			nil nil)
		       ("forslice"
			"for idx, item := range ${1:slice} {\n    $0\n}"
			"forslice" nil nil nil
			"/home/tychoish/.emacs.d/snippets/go-mode/forslice"
			nil nil)
		       ("formp"
			"for key, value := range ${1:mp} {\n    $0\n}"
			"formap" nil nil nil
			"/home/tychoish/.emacs.d/snippets/go-mode/formap"
			nil nil)
		       ("ec" "ec := &erc.Collector{}\n" "ec" nil nil
			nil
			"/home/tychoish/.emacs.d/snippets/go-mode/ec"
			nil nil)
		       ("db" "\\`db:\"$1\"\\`" "db" nil nil nil
			"/home/tychoish/.emacs.d/snippets/go-mode/db"
			nil nil)
		       ("ctxwt"
			"ctx, cancel := context.WithTimeout(context.Background(), $1)\ndefer cancel()\n\n$0\n"
			"ctxwt" nil nil nil
			"/home/tychoish/.emacs.d/snippets/go-mode/ctxwt"
			nil nil)
		       ("ctxwc"
			"ctx, cancel := context.WithCancel(context.Background())\ndefer cancel()\n"
			"ctxwc" nil nil nil
			"/home/tychoish/.emacs.d/snippets/go-mode/ctxwc"
			nil nil)
		       ("ctxt" "ctx := t.Context()\n" "ctxt" nil nil
			nil
			"/home/tychoish/.emacs.d/snippets/go-mode/ctxt"
			nil nil)
		       ("ctxb" "context.Background()\n" "ctxb" nil nil
			nil
			"/home/tychoish/.emacs.d/snippets/go-mode/ctxb"
			nil nil)
		       ("cstring"
			"c${1:$(capitalize yas/text)} := C.CString($1)\ndefer C.free(unsafe.Pointer(c${1:$(capitalize yas/text)}))\n\n"
			"CString" nil nil ((yas/indent-line 'fixed))
			"/home/tychoish/.emacs.d/snippets/go-mode/cstring"
			nil nil)
		       ("new"
			"// New$1 returns a new $1.\nfunc New$1() *$1 {\n	$2 := &$1{$0}\n	return $2\n}\n"
			"for constructor" nil nil nil
			"/home/tychoish/.emacs.d/snippets/go-mode/constructor"
			nil nil)
		       ("case" "case $1:\n	$0" "for switch case"
			nil nil nil
			"/home/tychoish/.emacs.d/snippets/go-mode/case"
			nil nil)
		       ("bson" "\\`bson:\"$1\"\\`\n" "bson" nil nil
			nil
			"/home/tychoish/.emacs.d/snippets/go-mode/bson"
			nil nil)
		       ("append" "$1 = append($1, $0)" "for append"
			nil nil nil
			"/home/tychoish/.emacs.d/snippets/go-mode/append"
			nil nil)
		       ("<<"
			"$1 = append(${1:theSlice}, ${2:theValue})\n"
			"<<" nil nil nil
			"/home/tychoish/.emacs.d/snippets/go-mode/<<"
			nil nil)))


;;; Do not edit! File generated at Fri Aug  1 09:43:16 2025
