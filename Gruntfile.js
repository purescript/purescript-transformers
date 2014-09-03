module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({

    libFiles: [
      "src/**/*.purs",
      "bower_components/purescript-*/src/**/*.purs*"
    ],

    clean: ["output", "tmp"],

    pscMake: ["<%=libFiles%>"],
    dotPsci: ["<%=libFiles%>"],
    docgen: {
        readme: {
            src: "src/**/*.purs",
            dest: "docs/Module.md"
        }
    },

    psc: {
      options: {
        main: true
      },
      exampleReader: {
        src: ["examples/Reader.purs", "<%=libFiles%>"],
        dest: "tmp/Reader.js"
      },
      exampleState: {
        src: ["examples/State.purs", "<%=libFiles%>"],
        dest: "tmp/State.js"
      },
      exampleWriter: {
        src: ["examples/Writer.purs", "<%=libFiles%>"],
        dest: "tmp/Writer.js"
      },
      exampleCont: {
        src: ["examples/Cont.purs", "<%=libFiles%>"],
        dest: "tmp/Cont.js"
      }
    },

    execute: {
      exampleReader: {
        src: "tmp/Reader.js"
      },
      exampleState: {
        src: "tmp/State.js"
      },
      exampleWriter: {
        src: "tmp/Writer.js"
      },
      exampleCont: {
        src: "tmp/Cont.js"
      }
    }

  });

  grunt.loadNpmTasks("grunt-purescript");
  grunt.loadNpmTasks("grunt-execute");
  grunt.loadNpmTasks("grunt-contrib-clean");

  grunt.registerTask("exampleReader", ["psc:exampleReader", "execute:exampleReader"]);
  grunt.registerTask("exampleState", ["psc:exampleState", "execute:exampleState"]);
  grunt.registerTask("exampleWriter", ["psc:exampleWriter", "execute:exampleWriter"]);
  grunt.registerTask("exampleCont", ["psc:exampleCont", "execute:exampleCont"]);
  grunt.registerTask("examples", ["psc", "execute"]);
  grunt.registerTask("make", ["pscMake", "dotPsci", "docgen"]);
  grunt.registerTask("default", ["make"]);
};
