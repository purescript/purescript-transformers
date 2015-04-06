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
    pscDocs: {
        trans: {
            src: "src/Control/Monad/Trans.purs",
            dest: "docs/Monad/Trans.md"
        },
        cont: {       
            src: "src/Control/Monad/Cont/*.purs",
            dest: "docs/Monad/Cont.md"
        },
        error: {
            src: ["src/Control/Monad/Error/*.purs", "src/Control/Monad/Error.purs"],
            dest: "docs/Monad/Error.md"
        },
        maybe: {
            src: "src/Control/Monad/Maybe/*.purs",
            dest: "docs/Monad/Maybe.md"
        },
        reader: {
            src: ["src/Control/Monad/Reader/*.purs", "src/Control/Monad/Reader.purs"],
            dest: "docs/Monad/Reader.md"
        },
        rws: {
            src: ["src/Control/Monad/RWS/*.purs", "src/Control/Monad/RWS.purs"],
            dest: "docs/Monad/RWS.md"
        },
        state: {
            src: ["src/Control/Monad/State/*.purs", "src/Control/Monad/State.purs"],
            dest: "docs/Monad/State.md"
        },
        writer: {
            src: ["src/Control/Monad/Writer/*.purs", "src/Control/Writer/Reader.purs"],
            dest: "docs/Monad/Writer.md"
        },
        cotrans: {
            src: "src/Control/Comonad/Trans.purs",
            dest: "docs/Comonad/Trans.md"
        },
        env: {       
            src: ["src/Control/Comonad/Env/*.purs", "src/Control/Comonad/Env.purs"],
            dest: "docs/Comonad/Env.md"
        },
        store: {
            src: ["src/Control/Comonad/Store/*.purs", "src/Control/Comonad/Store.purs"],
            dest: "docs/Comonad/Store.md"
        },
        traced: {
            src: ["src/Control/Comonad/Traced/*.purs", "src/Control/Comonad/Traced.purs"],
            dest: "docs/Comonad/Traced.md"
        },
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
      exampleStateEff: {
        src: ["examples/StateEff.purs", "<%=libFiles%>"],
        dest: "tmp/StateEff.js"
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
      exampleStateEff: {
        src: "tmp/StateEff.js"
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
  grunt.registerTask("exampleStateEff", ["psc:exampleStateEff", "execute:exampleStateEff"]);
  grunt.registerTask("exampleWriter", ["psc:exampleWriter", "execute:exampleWriter"]);
  grunt.registerTask("exampleCont", ["psc:exampleCont", "execute:exampleCont"]);
  grunt.registerTask("examples", ["psc", "execute"]);
  
  grunt.registerTask("make", ["pscMake", "dotPsci", "pscDocs"]);
  grunt.registerTask("default", ["make"]);
};
