"use strict";

var gulp = require("gulp");
var plumber = require("gulp-plumber");
var purescript = require("gulp-purescript");
var jsvalidate = require("gulp-jsvalidate");
var run = require("gulp-run");

var paths = [
  "src/**/*.purs",
  "bower_components/purescript-*/src/**/*.purs"
];

gulp.task("make", function() {
  return gulp.src(paths)
    .pipe(plumber())
    .pipe(purescript.pscMake());
});

gulp.task("jsvalidate", ["make"], function () {
  return gulp.src("output/**/*.js")
    .pipe(plumber())
    .pipe(jsvalidate());
});

var docTasks = [];

var docTask = function(name) {
  var taskName = "docs-" + name.toLowerCase();
  gulp.task(taskName, function () {
    var path = "src/" + name.replace(/\./g, "/") + "*";
    return gulp.src([path + "*.purs", path + "/**/*.purs"])
      .pipe(plumber())
      .pipe(purescript.pscDocs())
      .pipe(gulp.dest("docs/" + name + ".md"));
  });
  docTasks.push(taskName);
};

["Control.Monad.Cont", "Control.Monad.Error", "Control.Monad.Maybe",
 "Control.Monad.Reader", "Control.Monad.RWS", "Control.Monad.State",
 "Control.Monad.Trans", "Control.Monad.Writer", "Control.Comonad.Env",
 "Control.Comonad.Store", "Control.Comonad.Traced", "Control.Comonad.Trans"
 ].forEach(docTask);

gulp.task("docs", docTasks);

var exampleTasks = [];

var exampleTask = function(name) {
  var taskName = "example-" + name.toLowerCase();
  gulp.task(taskName, function() {
    return gulp.src(["examples/" + name + ".purs"].concat(paths))
      .pipe(plumber())
      .pipe(purescript.psc({ main: "Example." + name }))
      .pipe(run("node"));
  });
  exampleTasks.push(taskName);
};

["Cont", "Reader", "State", "StateEff", "Writer"].forEach(exampleTask);

gulp.task("examples", exampleTasks);

gulp.task("default", ["jsvalidate", "docs", "examples"]);
