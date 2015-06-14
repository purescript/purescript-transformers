/* jshint node: true */
"use strict";

var gulp = require("gulp");
var plumber = require("gulp-plumber");
var purescript = require("gulp-purescript");
var rimraf = require("rimraf");
var run = require("gulp-run");

var sources = [
  "src/**/*.purs",
  "bower_components/purescript-*/src/**/*.purs",
  "examples/*.purs"
];

var foreigns = [
  "bower_components/purescript-*/src/**/*.js"
];

gulp.task("clean-docs", function (cb) {
  rimraf("docs", cb);
});

gulp.task("clean-output", function (cb) {
  rimraf("output", cb);
});

gulp.task("clean", ["clean-docs", "clean-output"]);

gulp.task("make", function() {
  return gulp.src(sources)
    .pipe(plumber())
    .pipe(purescript.pscMake({ ffi: foreigns }));
});

gulp.task("docs", ["clean-docs"], function () {
  return gulp.src(sources)
    .pipe(plumber())
    .pipe(purescript.pscDocs({
      docgen: {

        "Control.Comonad.Env": "docs/Control.Comonad.Env.md",
        "Control.Comonad.Env.Class": "docs/Control.Comonad.Env.md",
        "Control.Comonad.Env.Trans": "docs/Control.Comonad.Env.md",

        "Control.Comonad.Store": "docs/Control.Comonad.Store.md",
        "Control.Comonad.Store.Class": "docs/Control.Comonad.Store.md",
        "Control.Comonad.Store.Trans": "docs/Control.Comonad.Store.md",

        "Control.Comonad.Traced": "docs/Control.Comonad.Traced.md",
        "Control.Comonad.Traced.Class": "docs/Control.Comonad.Traced.md",
        "Control.Comonad.Traced.Trans": "docs/Control.Comonad.Traced.md",

        "Control.Comonad.Trans": "docs/Control.Comonad.Trans.md",

        "Control.Monad.Cont.Class": "docs/Control.Monad.Cont.md",
        "Control.Monad.Cont.Trans": "docs/Control.Monad.Cont.md",

        "Control.Monad.Error": "docs/Control.Monad.Error.md",
        "Control.Monad.Error.Class": "docs/Control.Monad.Error.md",
        "Control.Monad.Error.Trans": "docs/Control.Monad.Error.md",

        "Control.Monad.Except": "docs/Control.Monad.Except.md",
        "Control.Monad.Except.Trans": "docs/Control.Monad.Except.md",

        "Control.Monad.List.Trans": "docs/Control.Monad.List.Trans.md",
        "Control.Monad.Maybe.Trans": "docs/Control.Monad.Maybe.Trans.md",

        "Control.Monad.Reader": "docs/Control.Monad.Reader.md",
        "Control.Monad.Reader.Class": "docs/Control.Monad.Reader.md",
        "Control.Monad.Reader.Trans": "docs/Control.Monad.Reader.md",

        "Control.Monad.RWS": "docs/Control.Monad.RWS.md",
        "Control.Monad.RWS.Class": "docs/Control.Monad.RWS.md",
        "Control.Monad.RWS.Trans": "docs/Control.Monad.RWS.md",

        "Control.Monad.State": "docs/Control.Monad.State.md",
        "Control.Monad.State.Class": "docs/Control.Monad.State.md",
        "Control.Monad.State.Trans": "docs/Control.Monad.State.md",

        "Control.Monad.Trans": "docs/Control.Monad.Trans.md",

        "Control.Monad.Writer": "docs/Control.Monad.Writer.md",
        "Control.Monad.Writer.Class": "docs/Control.Monad.Writer.md",
        "Control.Monad.Writer.Trans": "docs/Control.Monad.Writer.md"
      }
    }));
});

gulp.task("dotpsci", function () {
  return gulp.src(sources)
    .pipe(plumber())
    .pipe(purescript.dotPsci());
});

gulp.task("test", function() {
  return gulp.src(sources.concat(["test/**/*.purs", "examples/**/*.purs"]))
    .pipe(plumber())
    .pipe(purescript.psc({ main: "Test.Main", ffi: foreigns }))
    .pipe(run("node"));
});

gulp.task("default", ["make", "docs", "dotpsci", "test"]);
